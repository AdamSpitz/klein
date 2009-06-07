 '$Revision:$'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         buffer = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces buffer.

CopyDowns:
globals kleinAndYoda memoryInterfaces abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (byteVector copy)'
        
         buffer <- byteVector copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces buffer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: 1 WithIndexDo: [|:x|
              buffer at: x Put: b IfAbsent: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: bv size WithIndexDo: [|:x|
              buffer
                copyRangeDstPos: x
                SrcArray: bv
                SrcPos: 0
                Len: bv size
                IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
            [todo optimization david].
            "try cIntSize: etc. primitive like in wordAt:IfFail: 8/05"

            ensureRegionIsBufferedAt: i Size: oopSize WithIndexDo: [|:x|
              theVM myAssemblerSystem
                 store: s AsByteVectorInto: buffer
                                       At: x
                                   IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: 1 WithIndexDo: [|:x|
              buffer at: x IfAbsent: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: n WithIndexDo: [|:x|
              (byteVector copySize: n)
                copyRangeDstPos: 0
                SrcArray: buffer
                SrcPos: x
                Len: n
                IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy buffer: buffer copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSpace: space = ( |
            | 
            copy setupNewBufferAt: space bottom Size: space sizeOfEntireRegion).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | 
            start + size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fComment: Ensures that the specified region is available in the buffer.\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureRegionIsBufferedAt: i Size: n WithIndexDo: blk IfFail: fb = ( |
            | 
            "hook for children"
            blk value: indexFor: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         growUpwardsTo: n = ( |
            | 
            buffer: buffer copySize: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         indexFor: addr = ( |
            | 
            addr - start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         rangeString = ( |
            | 
            start printString, '-', end printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateBy: d = ( |
            | 
            relocateTo: start + d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: s = ( |
            | start: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         setupNewBufferAt: base Size: s = ( |
            | 
            buffer: byteVector copySize: s.
            start: base.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         size = ( |
            | buffer size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'on a buffer (', buffer size printString, ' bytes)').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: tags\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAt: i = ( |
            | 
            [todo cleanup verifying "3??? relies on low tagging -- dmu 9/05"].
            vmKit tag tagOfOop: byteAt: i + 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         totalByteSizeOfBuffers = ( |
            | 
            buffer size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            "add to turn into smi"
            ensureRegionIsBufferedAt: i Size: oopSize IfFail: [|:e| ^ fb value: e].

            int32 add: 0 
                 With: theVM myAssemblerSystem
                         intNNFromBytes: buffer
                                     At: (indexFor: i)
                                 IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         writeAllPiecesTo: aForeignProcess IfFail: fb = ( |
            | 
            aForeignProcess write: buffer ToMemoryAt: start IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> () From: ( | {
         'Comment: read by relocateTo: in vmImage\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         start <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces foreignProcess.

CopyDowns:
globals kleinAndYoda memoryInterfaces abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (nil)'
        
         foreignProcess.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces foreignProcess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateWords: nw = ( |
            | 
            foreignProcess allocateMemorySize: nw * oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         allowInfiniteSlopDuring: blk = ( |
            | 
            foreignProcess allowInfiniteSlopDuring: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         architecture = ( |
            | 
            foreignProcess
             ifNil:    'none yet'
             IfNotNil: [foreignProcess architecture]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
            | foreignProcess write: bv ToMemoryAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
            foreignProcess 
              writeWord: w
              ToMemoryAt: i 
              IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
            | foreignProcess readMemoryAt: i Size: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesBypassingCacheAt: i Size: n IfFail: fb = ( |
            | 
            foreignProcess readMemoryBypassingCacheAt: i Size: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForForeignProcess: fp = ( |
            | copy foreignProcess: fp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForForeignProcess: fp SnarfingBufferFrom: aBufferMemoryInterface = ( |
            | 
            copy initializeForForeignProcess: fp SnarfingBufferFrom: aBufferMemoryInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         hostName = ( |
            | 
            foreignProcess
             ifNil:    'nowhere yet'
             IfNotNil: [foreignProcess hostName]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForForeignProcess: fp SnarfingBufferFrom: aBufferMemoryInterface = ( |
            | 
            foreignProcess: fp.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCaches = ( |
            | 
            foreignProcess invalidateCaches.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateObsoleteCachedItemsIn: cachingWobulator = ( |
            | 
            foreignProcess invalidateObsoleteCachedItemsIn: cachingWobulator.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         isForForeignProcess = ( |
            | 
            [todo cleanup oraclesAndOIDs].
            "This is a HACK. I want to be able to get at the foreignProcess
             WITHOUT going through the memoryInterface. -- Adam"

            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            foreignProcess
             ifNil:    'none yet'
             IfNotNil: [foreignProcess myAssemblerSystem]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'on ', hostName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            foreignProcess
              readMemoryWordAt: i
                        IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: iterating by word\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordsAt: addr Size: n Do: blk IfFail: fb = ( |
            | 
            "Go in smallish chunks so that we don't hold the foreignProcess's
             semaphore for too long. Is there a better solution? -- Adam, 7/05"
            wordsAt: addr Size: n Do: blk InChunksOfSize: 10000 IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: iterating by word\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsAt: firstAddress Size: n Do: blk InChunksOfSize: chunkSize IfFail: fb = ( |
             endAddress.
            | 
            endAddress: firstAddress + (n * oopSize).
            firstAddress upTo: endAddress By: chunkSize Do: [|:chunkStart|
              foreignProcess
                readMemoryWordsAt: chunkStart
                             Size: (chunkSize min: endAddress - chunkStart) / oopSize
                               Do: blk
                           IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bufferedForeignProcess = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces foreignProcess copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces bufferedForeignProcess.

CopyDowns:
globals kleinAndYoda memoryInterfaces foreignProcess. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (byteVector copy)'
        
         buffer <- byteVector copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)'
        
         bufferStart <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (false)'
        
         isDirty <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( |
             {} = 'Comment: Allows reading and writing to a buffer.
And flushing to a real external memory.
No clever synchronization.\x7fModuleInfo: Creator: globals kleinAndYoda memoryInterfaces bufferedForeignProcess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: 1 IfFail: [|:e| ^ fb value: e].
            buffer at: (indexFor: i) Put: b IfAbsent: [|:e| ^ fb value: e].
            beDirty).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: bv size IfFail: [|:e| ^ fb value: e].
            buffer
              copyRangeDstPos: (indexFor: i)
              SrcArray: bv
              SrcPos: 0
              Len: bv size
              IfFail: [|:e| ^ fb value: e].
            beDirty).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: oopSize IfFail: [|:e| ^ fb value: e].
            theVM myAssemblerSystem
                          store: w
               AsByteVectorInto: buffer
                             At: (indexFor: i)
                         IfFail: [|:e| ^ fb value: e].
            beDirty).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         beDirty = ( |
            | 
            isEmpty ifFalse: [isDirty: true].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: 1 IfFail: [|:e| ^ fb value: e].
            buffer at: (indexFor: i) IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
            | 
            ensureRegionIsBufferedAt: i Size: n IfFail: [|:e| ^ fb value: e].
            (byteVector copySize: n)
              copyRangeDstPos: 0
              SrcArray: buffer
              SrcPos: (indexFor: i)
              Len: n
              IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fComment: Ensures that the specified region is available in the buffer.\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureRegionIsBufferedAt: i Size: n IfFail: fb = ( |
            | 
            "could be more clever here -- jb 5/03"
            isEmpty ifTrue: [fill].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fComment: Fills all buffered contents from the foreign process.\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         fill = ( |
            | 
            mustBeClean.
            (foreignProcess allocatedRegions size > 1) ifTrue: [
              ^ error: 'can only handle one allocated region'
            ].
            removeAll.
            foreignProcess allocatedRegions isEmpty ifTrue: [^ self].
            setupBufferFromRegion: foreignProcess allocatedRegions first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fComment: Flushes all buffered contents to the foreign process.\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         flush = ( |
            | 
            isDirty ifFalse: [^ self].
            foreignProcess write: buffer
                      ToMemoryAt: bufferStart
                          IfFail: raiseError.
            isDirty: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         indexFor: addr = ( |
            | 
            addr - bufferStart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            removeAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForForeignProcess: fp SnarfingBufferFrom: aBufferMemoryInterface = ( |
            | 
            resend.initializeForForeignProcess: fp
                    SnarfingBufferFrom: aBufferMemoryInterface.
            snarfBufferFrom: aBufferMemoryInterface.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         isEmpty = ( |
            | buffer isEmpty).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         mustBeClean = ( |
            | 
            isDirty ifTrue: [error: 'should have done a flush when you were finished writing'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'foreignProcess' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fComment: Discards all buffered contents without replacement
including anything that might not yet have been flushed.
The buffer becomes empty.\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAll = ( |
            | 
            buffer: buffer copyRemoveAll.
            isDirty: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         setupBuffer: aByteVector At: addr = ( |
            | 
            isEmpty not ifTrue: [^ error: 'can only handle one allocated region'].
            buffer: aByteVector.
            bufferStart: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         setupBufferFromRegion: aRegion = ( |
             addr.
             n.
            | 
            addr: aRegion x.
            n: aRegion y - aRegion x.
            setupBuffer: (foreignProcess readMemoryAt: addr Size: n IfFail: [raiseError])
                     At: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         setupNewBufferAt: addr Size: s = ( |
            | 
            setupBuffer: (byteVector copySize: s) At: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         snarfBufferFrom: aBufferMemoryInterface = ( |
            | 
            mustBeClean.
            setupBuffer: aBufferMemoryInterface buffer
                     At: aBufferMemoryInterface start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 'on a buffer for ', hostName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'bufferedForeignProcess' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            "add to turn into smi"
            ensureRegionIsBufferedAt: i Size: oopSize IfFail: [|:e| ^ fb value: e].

            int32 add: 0 
                 With:  theVM myAssemblerSystem
                          intNnFromBytes: buffer 
                                      At: (indexFor: i)
                                  IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         composite = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces composite.

CopyDowns:
globals kleinAndYoda memoryInterfaces abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (kleinAndYoda memoryInterfaces buffer)\x7fVisibility: private'
        
         high <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (kleinAndYoda memoryInterfaces buffer)\x7fVisibility: private'
        
         low <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces composite parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b IfFail: fb = ( |
            | 
            at: i
            Size: 1
            WithInterfaceDo: [|:mi| mi at: i PutByte: b IfFail: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
            | 
            at: i 
            Size: bv size
            WithInterfaceDo: [|:mi| mi at: i PutBytes: bv IfFail: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
            at: i
            Size: oopSize
            WithInterfaceDo: [|:mi| mi at: i PutWord: w IfFail: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         at: i Size: n WithInterfaceDo: blk IfFail: fb = ( |
             j.
            | 
            j: i + n.
            j <= low end ifTrue: [blk value: low]
                          False: [blk value: high]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i IfFail: fb = ( |
            | 
            at: i
            Size: 1
            WithInterfaceDo: [|:mi| mi byteAt: i IfFail: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
            | 
            at: i
            Size: n
            WithInterfaceDo: [|:mi| mi bytesAt: i Size: n IfFail: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLowMemory: lowMem HighMemory: highMem = ( |
            | 
            (resend.copy
              low:  lowMem)
              high: highMem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | 
            high end).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: composed memory interfaces\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         memoryInterfacesDo: blk = ( |
            | 
            blk value:  low.
            blk value: high.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         rangeString = ( |
            | 
            low rangeString, ' ', high rangeString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: s = ( |
             d.
            | 
            d: s - start.
             low relocateBy: d.
            high relocateBy: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         size = ( |
            | 
            high end - low start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            low start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'on buffers [',  rangeString, ']').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: tags\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAt: i = ( |
            | 
            at: i
            Size: oopSize
            WithInterfaceDo: [|:mi| mi tagAt: i]
            IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         totalByteSizeOfBuffers = ( |
            | 
            low totalByteSizeOfBuffers + high totalByteSizeOfBuffers).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            at: i
            Size: oopSize
            WithInterfaceDo: [|:mi| mi wordAt: i IfFail: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         writeAllPiecesTo: aForeignProcess IfFail: fb = ( |
            | 
            memoryInterfacesDo: [|:mi|
              mi writeAllPiecesTo: aForeignProcess IfFail: [|:e| ^ fb value: e].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         growingBuffer = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces growingBuffer.

CopyDowns:
globals kleinAndYoda memoryInterfaces abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: high\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (byteVector)\x7fVisibility: private'
        
         highBuffer <- bootstrap stub -> 'globals' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: high\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         highEnd <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: high\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         highStart <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: low\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         highestAccessedAddressInLowBuffer <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: low\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (byteVector)\x7fVisibility: private'
        
         lowBuffer <- bootstrap stub -> 'globals' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: low\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         lowEnd <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: low\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         lowStart <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: high\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (infinity)\x7fVisibility: private'
        
         lowestAccessedAddressInHighBuffer <- infinity.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         midpoint <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces growingBuffer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b IfFail: fb = ( |
            | 
            at: i
            Size: 1
            WithBufferAndIndexDo: [|:buf. :x| buf at: x Put: b IfAbsent: fb]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
            | 
            at: i 
            Size: bv size
            WithBufferAndIndexDo: [|:b. :x| 
              b
                copyRangeDstPos: x
                SrcArray: bv
                SrcPos: 0
                Len: bv size
                IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
            [todo optimization david].
            "try cIntSize: etc. primitive like in wordAt:IfFail: 8/05"

            at: i
            Size: oopSize
            WithBufferAndIndexDo: [|:b. :x| 
              theVM myAssemblerSystem
                store: w
                AsByteVectorInto: b
                              At: x
                          IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         at: i Size: n WithBufferAndIndexDo: blk IfFail: fb = ( |
             j.
            | 
            j: i + n.

            "Not only is it unnecessary to check if i < lowStart or j > highEnd,
             it will create a problem to do so.
             Unnecessary because the actual data load/stores (at:, at:Put:)
             will fail if need be.
             Will cause a problem because the bytesParts may try to store
             zero bytes right at the end. This should not fail. -- dmu 1/05"

            "Also, this is optimized for common cases first."

            case
             if: [ j <= lowEnd    ]  Then: [ justAccessedLowBufferFrom: i To: j.
                                             blk value:  lowBuffer With: i - lowStart]
             If: [ highStart <= i ]  Then: [ justAccessedHighBufferFrom: i To: j.
                                             blk value: highBuffer With: i - highStart.]
                                     Else: [ growBuffersToAllowAccessFrom: i To: j.
                                             at: i Size: n WithBufferAndIndexDo: blk IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single bytes\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i IfFail: fb = ( |
            | 
            at: i Size: 1 WithBufferAndIndexDo: [|:buf. :x| buf at: x IfAbsent: fb] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
            | 
            at: i
            Size: n
            WithBufferAndIndexDo: [|:b. :x|
              (byteVector copySize: n)
                copyRangeDstPos: 0
                SrcArray: b
                SrcPos: x
                Len: n
                IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         changeSizeOfHighBufferTo: n = ( |
             r.
            | 
            n > highBuffer size ifTrue: [
              r: byteVector copySize: n.
              r copyRangeDstPos: n - highBuffer size
                       SrcArray: highBuffer
                         SrcPos: 0
                           Len: highBuffer size.
            ] False: [
              r: highBuffer copyFrom: highBuffer size - n.
            ].
            highBuffer: r.
            highStart: highEnd - highBuffer size.
            updateMidpoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         changeSizeOfLowBufferTo: n = ( |
            | 
            lowBuffer: lowBuffer copySize: n.
            lowEnd: lowStart + lowBuffer size.
            updateMidpoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         copyAt: base Size: s = ( |
            | 
            (((((
            copy
              size: s)
              lowStart: base)
              lowEnd:   base)
              highStart: base + s)
              highEnd:   base + s)
              updateMidpoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: copying\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSpace: space = ( |
            | 
            copyAt: space bottom Size: space sizeOfEntireRegion).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | 
            highEnd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         growBuffersToAllowAccessFrom: i To: j = ( |
            | 
            case
              if: [(i < lowEnd) && [j > highStart]] Then: [trimBuffersToOnlyIncludeAccessedRegions.
                                                           growBuffersToAllowAccessFrom: i To: j]
              If: [ i < lowEnd                    ] Then: [growLowTo: j - lowStart]
              If: [                 j > highStart ] Then: [growHighTo: highEnd - i]
              If: [ i < midpoint                  ] Then: [growLowTo: j - lowStart]
                                                    Else: [growHighTo: highEnd - i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         growHighTo: n = ( |
            | 
            changeSizeOfHighBufferTo: newSizeFor: highBuffer ToGrowTo: n OtherBuffer: lowBuffer).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         growLowTo: n = ( |
            | 
            changeSizeOfLowBufferTo: newSizeFor: lowBuffer ToGrowTo: n OtherBuffer: highBuffer).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         justAccessedHighBufferFrom: i To: j = ( |
            | 
            lowestAccessedAddressInHighBuffer: lowestAccessedAddressInHighBuffer min: i.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         justAccessedLowBufferFrom: i To: j = ( |
            | 
            highestAccessedAddressInLowBuffer: highestAccessedAddressInLowBuffer max: j.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fCategory: helpers\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         minGrowth = 10000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fCategory: helpers\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         newSizeFor: buf ToGrowTo: n OtherBuffer: other = ( |
            | 
            [todo robustification].
            "can potentially run out of space if other has grown too much
             and we need the space for this one -- dmu 1/05"
            "If we kept track of the lowest address we've seen accessed in the
             high buffer, and vice-versa in the low buffer, we could trim them back
             if we need to."
            "The failure happens in " [updateMidpoint].
            size - other size min: n + minGrowth max: buf size double).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr IfFail: fb = ( |
            | 
            "optimization: addr is known to be in oops area"
            oopAtIndex: addr - lowStart IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         oopAtIndex: i IfFail: fb = ( |
            | 
            wordFrom: lowBuffer AtIndex: i IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         rangeString = ( |
            | 
            start printString, '-', end printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: s = ( |
             d.
            | 
            d: s - lowStart.
            lowStart:  lowStart  + d.
            lowEnd:    lowEnd    + d.
            midpoint:  midpoint  + d.
            highStart: highStart + d.
            highEnd:   highEnd   + d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | lowStart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'on two buffers [', lowStart printString, '-', lowEnd printString, ' ',
            highStart printString, '-', highEnd printString, ']').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: tags\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAt: addr = ( |
            | 
            tagAtIndex: addr - lowStart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: tags\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAtIndex: i = ( |
            | 
            [todo cleanup verifying "3??? relies on low tagging -- dmu 9/05"].
            vmKit tag tagOfOop: lowBuffer at: i + 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         totalByteSizeOfBuffers = ( |
            | 
            lowBuffer size + highBuffer size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         trimBuffersToOnlyIncludeAccessedRegions = ( |
            | 
            changeSizeOfLowBufferTo:  highestAccessedAddressInLowBuffer - lowStart.
            changeSizeOfHighBufferTo: highEnd - lowestAccessedAddressInHighBuffer.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fCategory: helpers\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         updateMidpoint = ( |
            | 
            [newSizeFor: buf ToGrowTo: n OtherBuffer: other]. "browsing"
            lowEnd <= highStart ifFalse: [error: 'out of space; implement fix described in newSizeFor:ToGrowTo:OtherBuffer:'].
            midpoint: lowEnd asInteger mean: highStart asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            at: i
            Size: oopSize
            WithBufferAndIndexDo: [|:b. :x| wordFrom: b AtIndex: x IfFail: fb ]
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         wordFrom: buffer AtIndex: i IfFail: fb = ( |
            | 
            (theVM myAssemblerSystem
              theVM myAssemblerSystem intNNFromBytes: buffer At: i) asSmallIntegerIfPossible).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         writeAllPiecesTo: aForeignProcess IfFail: fb = ( |
            | 
            aForeignProcess write:  lowBuffer ToMemoryAt:  lowStart IfFail: [|:e| ^ fb value: e].
            aForeignProcess write: highBuffer ToMemoryAt: highStart IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> () From: ( | {
         'Category: immutable\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         size <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         growingSingleBuffer = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces buffer copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces growingSingleBuffer.

CopyDowns:
globals kleinAndYoda memoryInterfaces buffer. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces growingSingleBuffer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fComment: Ensures that the specified region is available in the buffer.\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureRegionIsBufferedAt: addr Size: n WithIndexDo: blk IfFail: fb = ( |
             i.
             j.
            | 
            i: indexFor: addr.
            j: i + n.
            j > buffer size ifTrue: [growUpwardsTo: (j max: buffer size + minGrowth max: buffer size double) min: size].
            blk value: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         growUpwardsTo: n = ( |
            | 
            [n <= size] assert.
            resend.growUpwardsTo: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> 'parent' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         minGrowth = 10000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         setupNewBufferAt: base Size: s = ( |
            | 
            resend.setupNewBufferAt: base Size: 0. "It will grow when needed."
            size: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingSingleBuffer' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         size <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot'
        
         vmKitRmtMemIntrface = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitRmtMemIntrface.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitRmtMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitRmtMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitRmtMemIntrface postFileIn
