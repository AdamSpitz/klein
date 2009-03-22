 '$Revision: 30.85 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: browsing\x7fCategory: indirection\x7fCategory: Follow Pointer\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         followPointer = ( |
            | target model followPointer: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: showing and hiding\x7fCategory: Hide Me\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         hideMe = ( |
            | target model hideMe: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignRegisterSlotModel parent buttonDescriptions parent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Category: caching a window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (byteVector)\x7fVisibility: private'
        
         cachedMemory <- bootstrap stub -> 'globals' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Category: mapping pcs <-> line numbers\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         isCommentByPC <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Comment: Like external better.\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isInternal <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (true)\x7fVisibility: public'
        
         isSpecific <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Category: mapping pcs <-> line numbers\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         linesByPC <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myActivation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Category: caching a window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         myPCOfFirstCachedInstruction <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Category: caching a window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         myPCPastLastCachedInstruction <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( |
             {} = 'Comment: I combine several functions:
breaking the source into lines,
caching a window of disassembled instructions,
and filling the cache when needed
by getting the instructions disassembled.
Most outliners use a textLines object that just breaks the source
into lines, but since each line is a disassembled instruction anyway,
I was created to integrate and optimized the functionality.
-- dmu 1/02\x7fModuleInfo: Creator: globals klein disassembledMethodText parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         allocatedRegionFor: pc = ( |
            | 
            myActivation allocatedRegionFor: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: sizing the cached window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         backwardsBytes = 2048.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         beGeneral = ( |
            | 
            isSpecific ifFalse: [^ self].
            isSpecific: false.
            invalidate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         beSpecific = ( |
            | 
            isSpecific ifTrue: [^self].
            isSpecific: true.
            invalidate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         cacheRelevantOperandsFor: aDisassembledInstruction = ( |
             ofs.
             ops.
            | 
            "just symbolic operands for now"
            aDisassembledInstruction isRealInstruction ifFalse: [^ self].
            ofs: aDisassembledInstruction instructionTemplate operandFields.
            ops: aDisassembledInstruction operands.
            ofs with: ops Do: [|:of. :op|  (of isSymbolicForValue: op) ifTrue: [symbolicOperands add: op ]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         clipRange: range ToMethodPC: pc = ( |
             indexableSize.
             s.
             sentinel.
            | 
            sentinel: (klein virtualMachines selfVM compilerPrototype copy architecture:
                          myActivation myProcess architecture) methodStartInstruction.
            s: pc.
            [|:exit|
              s < range x  ifTrue: [^ range].
              (myActivation readMemoryAt: s Size: sentinel size IfFail: [^ range]) = sentinel 
                ifTrue: exit.
              s: s - sentinel size
            ] loopExit.
            myVM setTheVMAndDo: [
              indexableSize: myActivation readMemoryWordAt: s - (klein layouts byteVector byteOffsetFromIndexableSizeToFirstByte: s IfFail: -1)
                                                    IfFail: maxSmallInt.
            ].
            s @ (range y min: s + indexableSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: computing selection\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         convertPCToSelection: pc = ( |
             lineNumber.
             none.
            | 
            none: ((0@0) & ((lines firstIfAbsent: '') size pred @ 0)) asVector.
            isError ifTrue: [^ none].
            lineNumber: lineIndexFor: pc.
            (  (0@lineNumber ) 
            & ((lines at: lineNumber IfAbsent: [^ none]) size @ lineNumber)) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (((resend.copy
             symbolicOperands: symbolicOperands copy)
             linesByPC: linesByPC copy)
             pcsByLine: pcsByLine copy)
             isCommentByPC: isCommentByPC copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForActivation: a = ( |
            | 
            (copy myActivation: a) update).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembleInstructionsIn: assembledBytes From: start UpTo: end = ( |
             d.
             di.
             lineList.
             nextPCToIndex.
             pcsByLineList.
             src.
            | 
            d: myActivation myAssemblerSystem disassembler copyOrigin: start.
            isSpecific ifTrue: [d beSpecific] False: [d beGeneral].

            lineList: list copyRemoveAll.
            pcsByLineList: list copyRemoveAll.

                linesByPC: vector copySize: end - start FillingWith: 0.
            isCommentByPC: vector copySize: end - start FillingWith: false "for the in-betweens".
            nextPCToIndex: start.

            symbolicOperands: set copyRemoveAll.

            d disassembledInstructionsIn: assembledBytes 
              Do: [|:di|
                  cacheRelevantOperandsFor: di.

                  di isComment ifTrue: [
                    lineList addLast: ''.
                    pcsByLineList addLast:
                      pcsByLineList isEmpty ifTrue: 0 False: [pcsByLineList last].
                  ].
                  lineList addLast: '16r', di start hexPrintString, ': ',
                                       (isInternal ifTrue: [di internalSource]
                                                    False: [di externalSource]).
                  pcsByLineList addLast: di start.

                  [nextPCToIndex < di end] whileTrue: [
                        linesByPC at: nextPCToIndex - start  Put: lineList size pred.
                    isCommentByPC at: nextPCToIndex - start  Put: di isComment.
                    nextPCToIndex: nextPCToIndex succ.
                  ].
            ].  
            lines:     lineList      asVector.
            pcsByLine: pcsByLineList asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         doesCachedRangeInclude: pc = ( |
            | 
            (pcOfFirstCachedInstruction <= pc)
            && [pc < pcPastLastCachedInstruction]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         flushAndDisassembleForPC: pc = ( |
             d.
             m.
             r.
            | 
            r: rangeForPC: pc.
            m: readMemoryForRange: r IfFail: [^ self].
            disassembleInstructionsIn: m From: r x UpTo: r y.
            myPCOfFirstCachedInstruction:  r x.
            myPCPastLastCachedInstruction: r y.
            cachedMemory: m.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: sizing the cached window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         forwardsBytes = 8192.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: getting debuggee state\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         getPCIfFail: fb = ( |
            | 
            myActivation pcIfFail: [|:e|
              setError:'could not get pc: ', e.
              ^ fb value: e
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: sizing the cached window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         instructionSize = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidate = ( |
            | 
            myPCOfFirstCachedInstruction: 0.
            myPCPastLastCachedInstruction: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         isCacheValidForPC: pc = ( |
             m.
            | 
            (doesCachedRangeInclude: pc) ifFalse: [^ false].
            m: readMemoryForPC: pc.
            m = cachedMemory).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         isCommentAtPC: pc = ( |
            | 
            (doesCachedRangeInclude: pc) ifFalse: [update].
            isCommentByPC  at:  pc - myPCOfFirstCachedInstruction  IfAbsent: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: error remembering and reporting\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         isError = ( |
            | 
                (pcOfFirstCachedInstruction = 0) 
            && [pcPastLastCachedInstruction = -1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: computing selection\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         lineIndexFor: pc = ( |
            | 
            linesByPC at:  ((pc - pcOfFirstCachedInstruction) asSmallIntegerIfFail: [^ 1]) 
                IfAbsent: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         myVM = ( |
            | 
            myActivation myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         oneLiner = ( |
            | 
            update.
            isError ifTrue: [^ asString].
            textLineForPC: getPCIfFail: [^ asString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'abstractMethodText' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: computing selection\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         pcForLineIndex: n = ( |
            | 
            pcsByLine at: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: computing selection\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         pcForPoint: gridLocation = ( |
             lineIndex.
            | 
            lineIndex: gridLocation y.
            pcForLineIndex: lineIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOfFirstCachedInstruction = ( |
            | 
            myPCOfFirstCachedInstruction).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         pcPastLastCachedInstruction = ( |
            | 
            myPCPastLastCachedInstruction).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         rangeForPC: pc = ( |
             range.
             region.
            | 
            region: allocatedRegionFor: pc.
            range: unclippedRangeFor: pc.
            range: (range x  max: region x) @ (range y min: region y).
            [todo kleinDebugger]. "make sure range isn't too small when jumping into middle garbage -- dmu 6/05"
            clipRange: range ToMethodPC: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         readMemoryForPC: pc = ( |
            | 
            readMemoryForRange: (rangeForPC: pc) IfFail: byteVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: getting debuggee state\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         readMemoryForRange: r IfFail: fb = ( |
            | 
            myActivation readMemoryAt: r x Size: r y - r x
              IfFail: [|:e| 
                setError: 'could not read memory: ', e.
                ^ fb value: e
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: error remembering and reporting\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         setError: e = ( |
            | 
            myPCOfFirstCachedInstruction: 0.
            myPCPastLastCachedInstruction: -1.
            lines: vector copyAddFirst: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         showExternalForm = ( |
            | 
            isInternal ifFalse: [^self].
            isInternal: false.
            invalidate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         showInternalForm = ( |
            | 
            isInternal ifTrue: [^self].
            isInternal: true.
            invalidate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         textLineForPC: pc = ( |
            | 
            lines at: (lineIndexFor: pc) IfAbsent: '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         unclippedRangeFor: pc = ( |
             start.
            | 
            start: pc - (pc && windowBytes pred).
            (start - backwardsBytes) @ (start + forwardsBytes)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: caching window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         update = ( |
             pc.
            | 
            pc: getPCIfFail: [ ^ self ].
            (isCacheValidForPC: pc) ifTrue: [^self].
            flushAndDisassembleForPC: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> 'parent' -> () From: ( | {
         'Category: sizing the cached window of instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         windowBytes = 64.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'Category: mapping pcs <-> line numbers\x7fModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         pcsByLine <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: public'
        
         symbolicOperands <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: modes\x7fCategory: Show General Instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         beGeneral = ( |
            | 
            target model beGeneral).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: modes\x7fCategory: Show Specific Instructions\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         beSpecific = ( |
            | 
            target model beSpecific).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: operand selection\x7fCategory: Done Selecting Operands\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         hideOperandSelectors = ( |
            | 
            target model hideOperandSelectors: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelForeignActivationModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalActivationModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: modes\x7fCategory: Show External Form\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         showExternalForm = ( |
            | 
            target model showExternalForm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: modes\x7fCategory: Show Internal Form\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         showInternalForm = ( |
            | target model showInternalForm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: debugging\x7fCategory: memory\x7fCategory: Display memory location...\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         showMemoryLocation = ( |
            | target model showMemoryLocationEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: debugging\x7fCategory: memory\x7fCategory: Display memory words...\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         showMemoryWords = ( |
            | target model showMemoryWordsEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: debugging\x7fCategory: memory\x7fCategory: Show object...\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         showObject = ( |
            | 
            target model showObject: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: operand selection\x7fCategory: Select Operands\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         showOperandSelectors = ( |
            | 
            target model showOperandSelectors: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: controlling\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_attach = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: controlling\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_continue = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: weird\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_deny_attach = 31.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: controlling\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_detach = 11.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: weird\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_firstmach = 32.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: controlling\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_kill = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_read_d = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_read_i = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_read_u = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: controlling\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_step = 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: controlling\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_trace_me = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_write_d = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_write_i = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         pt_write_u = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         getBaseAndLength = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         getReturnHandler = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         mach = 100.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         miniping = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         ping = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         ptrace = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         signal = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         terminate = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         waitStatus = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         yoda = 102.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'runStates' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         halted = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'runStates' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         running = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'runStates' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         stopped = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'runStates' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         uninterruptible = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'runStates' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         waiting = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigabrt = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigabrt' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigabrt.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigabrt' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'abort'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigabrt' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigalrm = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigalrm' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigalrm.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigalrm' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'alarm clock'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigalrm' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 14.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigbus = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigbus' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigbus.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigbus' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'bus error'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigbus' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigchld = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigchld' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigchld.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigchld' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'to parent on child stop or exit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigchld' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 20.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigcont = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigcont' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigcont.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigcont' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'continue a stopped process'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigcont' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 19.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigemt = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigemt' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigemt.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigemt' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'emulator trap'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigemt' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigfpe = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigfpe' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigfpe.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigfpe' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'floating point exception'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigfpe' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sighup = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sighup' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sighup.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sighup' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'hangup'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sighup' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigill = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigill' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigill.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigill' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'illegal instruction'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigill' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         siginfo = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'siginfo' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals siginfo.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'siginfo' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'information request'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'siginfo' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 29.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigint = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigint' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigint.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigint' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'interrupt'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigint' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigio = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigio' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigio.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigio' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'input/output possible'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigio' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 23.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigkill = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigkill' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigkill.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigkill' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'kill'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigkill' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigpipe = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigpipe' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigpipe.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigpipe' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'write on pipe with no one to read it'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigpipe' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 13.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigprof = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigprof' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigprof.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigprof' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'profiling time alarm'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigprof' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 27.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigquit = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigquit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigquit.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigquit' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'quit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigquit' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigsegv = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigsegv' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigsegv.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigsegv' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'segmentation violation'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigsegv' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 11.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigstop = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigstop' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigstop.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigstop' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'sendable stop signal not from tty'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigstop' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 17.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigsys = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigsys' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigsys.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigsys' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'bad argument to system call'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigsys' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 12.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigterm = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigterm' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigterm.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigterm' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'software termination signal from kill'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigterm' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 15.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigtrap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigtrap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigtrap.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigtrap' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'trace trap'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigtrap' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigtstp = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigtstp' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigtstp.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigtstp' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'stop signal from tty'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigtstp' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 18.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigttin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigttin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigttin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigttin' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'to readers pgrp upon background tty read'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigttin' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 21.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigttou = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigttou' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigttou.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigttou' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'like TTIN for output fi (tp->t_local&LTOSTOP)'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigttou' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 22.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigusr1 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigusr1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigusr1.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigusr1' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'user defined signal 1'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigusr1' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 30.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigusr2 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigusr2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigusr2.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigusr2' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'user defined signal 2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigusr2' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 31.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigvtalrm = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigvtalrm' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigvtalrm.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigvtalrm' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'virtual time alarm'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigvtalrm' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 26.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigwinch = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigwinch' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigwinch.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigwinch' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'window size changes'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigwinch' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 28.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigxcpu = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigxcpu' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigxcpu.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigxcpu' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'exceeded CPU time limit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigxcpu' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 24.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sigxfsz = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigxfsz' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sigxfsz.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigxfsz' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'exceeded file size limit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sigxfsz' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 25.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         sugurg = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sugurg' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals sugurg.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sugurg' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         description = 'urgent condition on IO channell'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> 'sugurg' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         number = 16.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> 'hostNameAccepter' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         actionFrom: editor Target: target Event: evt = ( |
             s.
            | 
            s: editor contentsString.
            editor string: s.
            editor editMode: 0 false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> 'hostNameCanceler' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         actionFrom: editor Target: target Event: evt = ( |
            | 
            editor editMode: 0 false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: debugging\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         byteOffsetFromIndexableSizeToFirstByte: firstByteAddr IfFail: fb = ( |
            | 
            layouts bytesPart byteOffsetFromIndexableSizeToFirstByte).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: debugging\x7fModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         byteOffsetFromIndexableSizeToFirstByte: firstByteAddr IfFail: fb = ( |
            | 
            (firstByteAddr - (machineMemory beginningOfObjectContainingAddress: firstByteAddr)) - (indexableSizeField fixedIndex * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         kleinDB = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinDB.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.85 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinDB' -> () From: ( | {
         'ModuleInfo: Module: kleinDB InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinProxyClients
kleinReflection
kleinUI
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinProxyClients' From: 'applications/klein'
 bootstrap read: 'kleinReflection' From: 'applications/klein'
 bootstrap read: 'kleinUI' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinDB postFileIn
