 '$Revision: 30.11 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: memory interfaces\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractMemoryInterface = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda abstractMemoryInterface.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( |
             {} = 'Comment: Provides an abstract interface to a RAM-like storage
mechanism.  Intended for use with polymorphic code
parameterized on storage class, such as lenses.\x7fModuleInfo: Creator: globals kleinAndYoda abstractMemoryInterface parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         allowInfiniteSlopDuring: blk = ( |
            | 
            blk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b = ( |
            | at: i PutByte: b IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b IfFail: fb = ( |
            | at: i PutBytes: (byteVector copyAddFirst: b) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv = ( |
            | at: i PutBytes: bv IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: addr PutOop: oop = ( |
            | 
            at: addr PutOop: oop IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: addr PutOop: oop IfFail: fb = ( |
            | 
            at: addr PutWord: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w = ( |
            | 
            at: i PutWord: w IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
             [todo oopSizeDependent].
            "will need word that is oopSized and word that address sized"
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: marks\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         atOffset: wordOffset From: baseAddr PutMarkWithValue: mv IfFail: fb = ( |
            | 
            atOffset: wordOffset From: baseAddr PutOop: (layouts mark encode: mv) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         atOffset: wordOffset From: baseAddr PutOop: oop IfFail: fb = ( |
            | 
            at: baseAddr + (wordOffset * oopSize) PutOop: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: finding beginnings of objects\x7fComment: Is not guaranteed to work if addr points to the middle of the
bytes part of a byteVector. -- Adam, 4/06\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         beginningOfObjectContainingAddress: addr = ( |
             a.
            | 
            "Search backwards until we find the mark at the beginning of the object."
            a: addr roundDownTo: oopSize.
            [a: a - oopSize.
             layouts object isMark: oopAt: a IfFail: [|:e| ^ fb value: e]] whileFalse.
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i = ( |
            | byteAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single bytes\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i IfFail: fb = ( |
            | (bytesAt: i Size: 1 IfFail: [|:e| ^ fb value: e]) first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n = ( |
            | bytesAt: i Size: n IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesBypassingCacheAt: i Size: n = ( |
            | 
            bytesBypassingCacheAt: i Size: n IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: byte vectors\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesBypassingCacheAt: i Size: n IfFail: fb = ( |
            | 
            bytesAt: i Size: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         copyWordAtOffset: wordOffset From: srcBaseAddr ToSameOffsetFrom: dstBaseAddr IfFail: fb = ( |
             byteOffset.
             failBlock.
             w.
            | 
            [todo optimization gc]. "Create a localMemoryInterface version of this."
            failBlock: [|:e| ^ fb value: e].
            byteOffset: wordOffset * oopSize.
            w: wordAt: srcBaseAddr + byteOffset IfFail: failBlock.
            at: dstBaseAddr + byteOffset PutWord: w IfFail: failBlock.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: tags\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         ifOopAt: addr IsObject: objBlk IsMark: markValueBlk = ( |
            | 
            ifOopAt: addr IsObject: objBlk IsMark: markValueBlk IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: tags\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         ifOopAt: addr IsObject: objBlk IsMark: markValueBlk IfFail: fb = ( |
             r.
             tag.
            | 
            tag: tagAt: addr.
            "Got to avoid cloning. -- Adam, Mar. 2009"
            __BranchIfTrue: (tag _IntEQ: vmKit tag mark) To: 'mark'.
            r:  objBlk value:  oopAt: addr IfFail: fb.
            __BranchTo: 'done'.
            __DefineLabel: 'mark'.
            r:  markValueBlk value: markValueAt: addr IfFail: fb.
            __DefineLabel: 'done'.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCaches = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateObsoleteCachedItemsIn: cachingWobulator = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         isForForeignProcess = ( |
            | 
            [todo cleanup oraclesAndOIDs].
            "This is a HACK. I want to be able to get at the foreignProcess
             WITHOUT going through the memoryInterface. -- Adam"

            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: marks\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         isMarkAtOffset: wordOffset From: baseAddr = ( |
            | 
            isMarkAtOffset: wordOffset From: baseAddr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: marks\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         isMarkAtOffset: wordOffset From: baseAddr IfFail: fb = ( |
            | 
            ifOopAt: baseAddr + (wordOffset * oopSize) IsObject: false IsMark: true IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: marks\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueAt: addr IfFail: fb = ( |
            | 
            layouts mark valueOf:  oopAt: addr IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         mustAllowSlopDuring: blk = ( |
            | blk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr = ( |
            | 
            oopAt: addr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr IfFail: fb = ( |
            | 
            wordAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAtOffset: wordOffset From: baseAddr = ( |
            | 
            oopAtOffset: wordOffset From: baseAddr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAtOffset: wordOffset From: baseAddr IfFail: fb = ( |
            | 
            oopAt: baseAddr + (wordOffset * oopSize) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         optimizesWordAccesses = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: tags\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAt: addr = ( |
            | 
            tagAt: addr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: tags\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAt: addr IfFail: fb = ( |
            | 
            [todo cleanup verifying dmu "magic number dmu"].
            vmKit tag tagOfOop: byteAt: addr + 3 IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i = ( |
            | wordAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single words\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            [todo oopSizeDependent].
            "will need word that is oopSized and word that address sized"
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: iterating by word\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordsAt: addr Size: n Do: blk = ( |
            | 
            wordsAt: addr Size: n Do: blk IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: iterating by word\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         wordsAt: addr Size: n Do: blk IfFail: fb = ( |
            | 
            "Children can override to make more efficient."
            addr upTo: addr + (n * oopSize) By: oopSize Do: [|:a|
              blk value: (wordAt: a IfFail: [|:e| ^ fb value: e])
                   With: a
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         writeAllPiecesTo: aForeignProcess IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot'
        
         vmKitMemInterface = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitMemInterface.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemInterface InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitLclMemIntrface
vmKitRmtMemIntrface
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitLclMemIntrface' From: 'applications/klein'
 bootstrap read: 'vmKitRmtMemIntrface' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitMemInterface postFileIn
