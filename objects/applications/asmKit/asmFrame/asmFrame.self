 '$Revision: 30.52 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblerSystems = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         framework = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fCategory: registers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         allRegisters = ( |
            | 
            cachedAllRegisters ifNil: [
              cachedAllRegisters: (registerNameSpaces gather: [|:ns| registersForNameSpace: ns]) asVector.
              cachedAllRegisters
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         areCIntsSameAsPlatforms = ( |
            | 
            intNN isThisPlatformBigEndian = isBigEndian).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assembler = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework assembler.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         areCommentsEmitted <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         failBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'Comment: address of current instruction\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         locationCounter <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         maxLocationCounter <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (byteVector)\x7fVisibility: private'
        
         myAssembledBytes <- bootstrap stub -> 'globals' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'Comment: address of first instruction\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         origin <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework assembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         align: nBytes = ( |
            | 
            [(locationCounter % nBytes) = 0] whileFalse: [
              data8: 0
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         anchorAssembledBytes = ( |
            | 
            "put an element at the start of assembledBytes
             to ensure that result of assembledBytes corresponds
             to origin location"
            (myAssembledBytes desiredMinCapacity: 1) at: 0 Put: 0.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assembledBytes = ( |
            | 
            ensureLabelsResolved.
            uncheckedAssembledBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assembledBytesFrom: start UpTo: end = ( |
            | 
            ensureLabelsResolved.
            myAssembledBytes copyFrom: (assembledBytesIndexFor: start)
                                 UpTo:  assembledBytesIndexFor: end).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         assembledBytesIndexFor: loc = ( |
            | 
            loc - origin).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assembledBytesSize = ( |
            | 
            maxLocationCounter - origin).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         at: anOffset PutByte: aByte = ( |
            | 
            myAssembledBytes at:  anOffset
                             Put: aByte
                             IfAbsent: [growAssembledBytes.
                                        ^ at: anOffset PutByte: aByte].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         atLC: lc Do: blk = ( |
             oldLC.
             r.
            | 
            oldLC: locationCounter.
            locationCounter: lc.
            r: blk value: oldLC.
            locationCounter: oldLC.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         backpatchLabelAt: instLC To: newVal = ( |
             d.
             di.
             inst.
            | 
            d: myAssemblerSystem disassembler copyOrigin: instLC.
            d beGeneral.
            di: d disassembleBytes: myAssembledBytes At: instLC.
            di isRealInstruction ifFalse: [ ^ error: 'not an instruction'].
            di instructionTemplate operandFields asVector
                 findFirst: [|:of    | of isBackpatchable          ]
                 IfPresent: [|:of. :i| di operandAt: i Put: newVal ]
                  IfAbsent: [error: 'could not find field to patch'].

            reassemble: di At: di start.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         bindLabel: lbl = ( |
            | 
            bindLabel: lbl To: locationCounter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         bindLabel: lbl To: lc = ( |
            | 
            unresolvedLabels remove: lbl.
            lbl resolveTo: lc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToLabel: lbl = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         breakpoint: msg = ( |
            | 
            trap.
            "put comment AFTER trap for debugger status display"
            comment: msg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         comment: aStringOrBlock = ( |
             past.
             s.
            | 
            areCommentsEmitted ifFalse: [^ self].

            "Some comments are expensive to calculate, so allow
             the caller to pass in a block instead of a string.
             -- Adam, 10/04"
            s: aStringOrBlock value.
            past: newLabel.
            branchToLabel: past.
            commentSentinel.
            data32: s size.
            dataBytes: s.
            align: 4.
            bindLabel: past.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         commentSentinel = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (resend.copy
             myAssembledBytes: myAssembledBytes copy)
             unresolvedLabels: unresolvedLabels copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAssembledBytesInto: bv = ( |
            | 
            ensureLabelsResolved.
            bv copyRangeDstPos: 0 
                      SrcArray: myAssembledBytes
                        SrcPos: 0
                           Len: assembledBytesSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOrigin: o = ( |
            | 
            (copy locationCounter: o) origin: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
             c.
            | 
            c: clone.
            c myAssembledBytes: myAssembledBytes copyRemoveAll.
            c unresolvedLabels: unresolvedLabels copyRemoveAll.
            c locationCounter: 0.
            c origin: 0.
            c maxLocationCounter: 0.
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         data32: anInt = ( |
             o.
            | 
            o: offset.
            myAssemblerSystem bytesFromInt: anInt Do: [|:b. :i| at: o + i PutByte: b].
            incrementLocationCounterBy: 4.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         data8: aByte = ( |
            | 
            at: offset PutByte: aByte.
            incrementLocationCounterBy: 1.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         dataBytes: aByteVector = ( |
            | 
            myAssembledBytes copyRangeDstPos: offset  
                                    SrcArray: aByteVector
                                      SrcPos: 0
                                         Len: aByteVector size
                                      IfFail: [growAssembledBytes. ^ dataBytes: aByteVector].
            incrementLocationCounterBy: aByteVector size.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         defineLabel = ( |
            | defineLabelAt: locationCounter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         defineLabelAt: lc = ( |
            | 
            label copyForAsm: self ResolveTo: lc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureLabelsResolved = ( |
            | 
            [todo optimization]. "may find labels with no uses, cause we currently compile dead code."
            "Why not make the compiler smarter and not compile dead code someday? -- dmu 5/05"
            [todo cleanup dave]. "only do this if assertions are on, put flag in asm object, set from the one in the VM"
            unresolvedLabels findFirst: [|:lbl| lbl hasAnyUnresolvedUses]
                             IfPresent: [|:lbl| error: 'unresolved label created at ', lbl myValue printString].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         error: msg = ( |
            | 
            failBlock ifNil: [resend.error: msg] IfNotNil: [failBlock value: msg]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         growAssembledBytes = ( |
             newSize.
             oldSize.
            | 
            oldSize: myAssembledBytes size.
            newSize: oldSize = 0 
              ifTrue: 1000
               False: oldSize * 4.
            myAssembledBytes: myAssembledBytes copySize: newSize.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accumulating output\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         incrementLocationCounterBy: n = ( |
            | 
               locationCounter: locationCounter + n.
            maxLocationCounter: maxLocationCounter max: locationCounter.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         intNN = ( |
            | myAssemblerSystem intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         label = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework assembler parent label.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isResolved <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)'
        
         myAsm.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (16rbadbad)'
        
         myValue <- 16rbadbad.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework assembler parent label parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | resend.copy unresolvedUses: unresolvedUses copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForAsm: a = ( |
            | 
            ((copy 
             myAsm: a)
             myValue: a locationCounter)
             isResolved: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForAsm: a ResolveTo: val = ( |
            | 
            (((clone
             myAsm: a)
             myValue: val)
             isResolved: true)
             unresolvedUses: vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         hasAnyUnresolvedUses = ( |
            | 
            unresolvedUses isEmpty not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         resolveTo: val = ( |
            | 
            myValue: val.
            unresolvedUses do: [|:uu|
              uu backpatchTo: val With: myAsm.
            ].
            unresolvedUses removeAll.
            isResolved: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         resolvedValue = ( |
            | 
            [isResolved] assert.
            myValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         unresolvedUseProto = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework assembler parent label parent unresolvedUseProto.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)'
        
         instructionTemplate.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)'
        
         lc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework assembler parent label parent unresolvedUseProto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         backpatchTo: val With: a = ( |
            | 
            a atLC: lc Do: [
              instructionTemplate assemblyMethodSelector sendTo: a With: val
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> 'unresolvedUseProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         value: instTemplate With: lc = ( |
            | 
            isResolved ifTrue: [ ^ myValue].
            unresolvedUses add:  (unresolvedUseProto copy instructionTemplate: instTemplate) lc: lc.
            myValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         unresolvedUses <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> 'label' -> () From: ( | {
         'Comment: useful for debugging\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')'
        
         whereDidIComeFrom <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         newLabel = ( |
             r.
            | 
            r: label copyForAsm: self.
            unresolvedLabels add: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         offset = ( |
            | locationCounter - origin).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: patching\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         patchAddresses: addrs With: blk = ( |
             wasAt.
            | 
            wasAt: locationCounter.
            addrs do: [|:a|
              locationCounter: a.
              blk value.
            ].
            locationCounter: wasAt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: reassembling disassembled instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         reassemble: aDisassembledInstruction = ( |
             it.
            | 
            it: aDisassembledInstruction instructionTemplate.
            it ifNil: [error: 'disassembly failed'].
            it assemblyMethodSelector
              sendTo: self
              WithArguments: aDisassembledInstruction operands).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: reassembling disassembled instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         reassemble: aDI At: lc = ( |
            | 
            atLC: lc Do: [reassemble: aDI]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: saving code\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         saveBinaryAs: fileName = ( |
            | 
            fileName setFileContentsTo: assembledBytes asByteVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         uncheckedAssembledBytes = ( |
            | 
            myAssembledBytes copySize: assembledBytesSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: constants\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         zero = ( |
            | 
            data32: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         unresolvedLabels <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fCategory: mixins - children should inherit from either of these\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         bigEndianMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework bigEndianMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVectorFromInt: int = ( |
            | 
            [endianDependentServer].
            intNN asBigEndianByteVectorFrom: int).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFromInt: anInt Do: blk = ( |
            | 
            [endianDependentServer].
            intNN bigEndianBytesFrom: anInt Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         int32VectorFromBytes: bytes = ( |
            | 
            bytes asBigEndianInt32Vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         intNNFromBytes: instBytes At: offset IfFail: errBlk = ( |
            | 
            intNN copyTakeBigEndianBytesFrom:    instBytes Index: offset IfFail: errBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         isBigEndian = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         store: aWord AsByteVectorInto: bytes At: i IfFail: fb = ( |
            | 
            aWord storeAsBigEndianByteVectorInto: bytes At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         cleanAll = ( |
            | 
            clearCaches.
            generators do: [|:g| (g copyForAssemblerSystem: self) cleanAll].
            'cleanAll done.' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: caches\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         clearCaches = ( |
            | 
            cachedAllRegisters: nil.
            cachedAllRegisterLocations: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         conditionCodeRegisterName = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledInstruction = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework disassembledInstruction.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         commentOrErrorSource <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         end <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         instAsInt.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         instructionTemplate.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (vector)\x7fVisibility: public'
        
         operands <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework disassembledInstruction parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForInstructionTemplate: it Operands: ops Instruction: inst Start: s End: e = ( |
            | 
            ((((copy
              instructionTemplate: it)
              operands: ops asVector)
              instAsInt: inst)
              start: s)
              end: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForInstructionTemplate: it Operands: ops Start: s = ( |
            | 
            copyForInstructionTemplate: it
            Operands: ops
            Instruction: 0
            Start: s
            End: -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSource: s Start: st End: e = ( |
            | 
            ((copy
              commentOrErrorSource: s)
              start: st)
              end: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: source strings\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         externalHexPrefix = '0x'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: source strings\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         externalOpcode = ( |
            | 
            instructionTemplate externalName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: source strings\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         externalSource = ( |
            | 
            externalSourceShowingLCOffset: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: source strings\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         externalSourceShowingLCOffset: aBool = ( |
             ofs.
             r.
            | 
            commentOrErrorSource isEmpty ifFalse: [^ commentOrErrorSource].
            r: externalOpcode.
            ofs: instructionTemplate operandFields.
            ofs isEmpty ifTrue: [^r].
            r: r padOnRight: 5.
            operands with: ofs Do: [
              |:op. :of|
              r: r & ' ' & (of externalSourceFor: op
                                          IsLast: of = ofs last
                                              IT: instructionTemplate
                                              LC: start
                                        LCSymbol: locationCounterSymbol
                                    ShowLCOffset: aBool
                                       HexPrefix: externalHexPrefix).
            ].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: source strings\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         internalSource = ( |
             r.
            | 
            commentOrErrorSource isEmpty ifFalse: [^ commentOrErrorSource].
            r: instructionTemplate name.
            instructionTemplate operandFields isEmpty ifTrue: [^r].
            instructionTemplate operandFields asVector with:
                                operands      asVector Do: [|:of. :op|
              r: r, of keyword, ' ', (of sourceForValue: op value), ' '
            ].
            r copyWithoutLast).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         isComment = ( |
            | 
            isRealInstruction not && [instAsInt isNil]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         isRealInstruction = ( |
            | commentOrErrorSource isEmpty).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: source strings\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         locationCounterSymbol = '.'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'Category: mutating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         operandAt: i Put: op = ( |
            | 
            operands: operands copy at: i Put: op).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         start <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembler = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework disassembler.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         commentSentinel.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         isLookingForMostSpecific <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         isMyCByteOrderSameAsPlatforms.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         locationCounter <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework disassembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: setting specificity mode\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         beGeneral = ( |
            | isLookingForMostSpecific: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: setting specificity mode\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         beSpecific = ( |
            | isLookingForMostSpecific: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         buildCandidatesForTemplates: its Instruction: inst = ( |
            | 
            its copyMappedBy: [|:it. ops|
              ops:  it disassembleOperandsIn: inst At: locationCounter.
              myAssemblerSystem disassembledInstruction 
                copyForInstructionTemplate:  it
                                  Operands:  ops
                               Instruction:  inst
                                     Start:  locationCounter
                                       End:  locationCounter + it size
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         buildCommentSentinel = ( |
            | 
            commentSentinel: myAssemblerSystem intNNFromBytes: (myAssemblerSystem assembler copy) commentSentinel At: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fCategory: building sorted templates\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         buildSortedTemplates = ( |
            | 
            sortedTemplates: vector copySize: intNN bitSize succ.

            sortedTemplates do: [|:x. :i|
              sortedTemplates at: i Put: dictionary copyRemoveAll.
            ].

            myAssemblerSystem instructionTemplatesDo: [|:it|
              it isRedundant ifFalse: [
                (templatesMatchingIT: it) addLast: it.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOrigin: lc = ( |
            | 
            ( copy locationCounter: lc)
              isMyCByteOrderSameAsPlatforms: myAssemblerSystem areCIntsSameAsPlatforms).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         cullDisassembledInstructions: dis = ( |
             a.
            | 
            "some templates may have restrictions, so assemble them"
            a: myAssemblerSystem assembler copyOrigin: locationCounter.
            dis asList copyFilteredBy: [|:di|
              [|:exit| 
                a failBlock: [exit value: false].
                a reassemble: di At: di start.
                true
              ] exitValue
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassemble: inst = ( |
            | 
            disassembleParsingCommentsFromBytes: (myAssemblerSystem byteVectorFromInt: inst)
                                             At: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleAllExternalSource: instBytes = ( |
             r <- ''.
            | 
            disassembledInstructionsIn: instBytes Do: [|:di|
              r: r 
                 & di externalSource
                 & '\n'.
            ].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleAllExternalSourceWithAddresses: instBytes = ( |
             r <- ''.
            | 
            disassembledInstructionsIn: instBytes Do: [|:di|
              r: r 
                 & (di isComment ifTrue: '\n' False: '')
                 & '0x' & di start hexPrintString & ': '
                 & di externalSource
                 & '\n'.
            ].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleAllSource: instBytes = ( |
             r <- ''.
            | 
            disassembledInstructionsIn: instBytes Do: [|:di|
              r: r 
                 & di internalSource
                 & '\n'.
            ].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleBytes: instBytes At: offset = ( |
             di.
             dis.
             inst.
            | 
            inst: myAssemblerSystem intNNFromBytes: instBytes  At: offset.
            dis: disassembledInstructionsMatching: inst.
            dis isEmpty ifTrue: [^ disassembleIllegalInstructionFrom: inst].
            di: dis first.
            locationCounter: di end.
            di).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembleCommentFromBytes: bytes At: offset = ( |
             cmt.
             di.
             end.
             size.
             start.
             wordSize.
            | 
            wordSize: intNN size.
            start: offset + (3 * wordSize). "branch + sentinel + size"
            size: (myAssemblerSystem intNNFromBytes: bytes At: offset + wordSize double IfFail: maxSmallInt) asInteger.
            (start + size)  <=  bytes size
              ifFalse: [
                cmt: 'incomplete comment!'.
                end: bytes size roundDownTo: wordSize
              ]
              True: [
                cmt: bytes copyFrom: start UpTo: start + size.
                end: start + (size roundUpTo: wordSize)
              ].
            di: myAssemblerSystem disassembledInstruction 
              copyForSource: '// ', cmt Start: locationCounter End: (end - offset) + locationCounter.
            locationCounter: di end.
            di).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleExternalSourceIn: inst = ( |
             di.
            | 
            di: disassemble: inst.
            di externalSource).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembleIllegalInstructionFrom: inst = ( |
             di.
            | 
            di: myAssemblerSystem disassembledInstruction
                  copyForSource: inst shortIfPossibleHexPrintString
                          Start: locationCounter
                            End: locationCounter + intNN size.
            di instAsInt: inst.
            locationCounter: di end.
            di).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleParsingCommentsFromBytes: instBytes At: offset = ( |
             nextInst.
            | 
            nextInst: myAssemblerSystem intNNFromBytes: instBytes At: offset + intNN size IfFail: nil.
            "if nextInst is sentinal inst is just branch around comment, skip it"
            nextInst = commentSentinel  ifTrue: [^ disassembleCommentFromBytes: instBytes At: offset].
            disassembleBytes: instBytes At: offset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleSourceIn: inst = ( |
             di.
            | 
            di:  disassemble: inst.
            di internalSource).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledInstructionsIn: bytes At: offset Do: blk = ( |
             start.
            | 
            start: locationCounter.
            [((locationCounter - start) + offset + intNN size) <= bytes size] whileTrue: [
              blk value: disassembleParsingCommentsFromBytes: bytes
                                                          At: (locationCounter - start) + offset.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledInstructionsIn: instBytes Do: blk = ( |
            | 
            disassembledInstructionsIn: instBytes At: 0 Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembledInstructionsMatching: inst = ( |
             by.
             first.
             last.
             sits.
            | 
            sits: sortedTemplates.
            isLookingForMostSpecific ifTrue: [ first: sits  lastKey. last: sits firstKey. by: -1 ]
                                      False: [ first: sits firstKey. last: sits  lastKey. by:  1 ].
            first to: last By: by Do: [|:specificity. dis|
              dis: disassembledInstructionsMatching: inst ForMasks: sits at: specificity.
              dis isEmpty ifFalse: [^ dis].
            ].
            vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembledInstructionsMatching: inst ForMasks: maskGroup = ( |
             cands.
             templates.
            | 
            templates: templatesMatchingInstruction: inst ForMasks: maskGroup.
            cands: buildCandidatesForTemplates: templates Instruction: inst.
            cullDisassembledInstructions: cands).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            myAssemblerSystem intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForConditionCodeValue: n = ( |
            | 
            "could be just '16r', n hexPrintString"
            '<<stringForConditionCodeValue:>> needs to be implemented in: <<',
              asMirror name, '>>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fCategory: building sorted templates\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         templatesMatchingIT: it = ( |
            | 
            ((sortedTemplates at: it specificity)
                              at: it opcodeMask IfAbsentPut: [dictionary copyRemoveAll]) 
                              at: it opcode     IfAbsentPut: [list       copyRemoveAll]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> () From: ( | {
         'Category: template matching\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         templatesMatchingInstruction: inst ForMasks: maskGroup = ( |
            | 
            maskGroup gather: [|:opcodes. :mask. maskedInst |
              maskedInst: intNN and: inst With: mask.
              opcodes at:  maskedInst
                IfAbsent:  vector.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> () From: ( | {
         'Comment: Optimize the disassembler by storing templates sorted
by specificity. Each element in this collector is a collection of collections of
templates with the same mask. -- dmu 1/02

This is in the prototype because it is common to all disassembler\'s for a particular platform.
In Java, the framework disassembler class holds accessors that are overidden for each platform
to use statics in the platform-specific disassembler classes. -- dmu 6/02 \x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         sortedTemplates <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: name spaces\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         fields = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'fields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework fields.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         fieldsDo: blk = ( |
             r.
            | 
            (reflect: fields) do: [|:s|
              r: blk value: s contents reflectee With: s name].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAll = ( |
             wasClean.
            | 
            wasClean: modules asmGeneratedSlots isClean.

            "This goes really slowly if the incremental updater is
             listening for every define. -- Adam, 11/04"
            vmKit incrementalUpdater dontListenDuring: [
              generators do: [|:g. t| 
                  ('--- ', g asString , ': ---') printLine.
                  t: [(g copyForAssemblerSystem: self) generateAll] time.
                  (g asString, ' took ', t printString, 'ms') printLine
              ].
            ].
            wasClean ifTrue: [modules asmGeneratedSlots beClean].
            'generateAll done.' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         generators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent field.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( | {
         'Category: syntax for generating external test file\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\' \')'
        
         externalPrefix <- ' '.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( | {
         'Category: syntax for generating external test file\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\',\')'
        
         externalSeparator <- ','.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( | {
         'Category: syntax for generating external test file\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')'
        
         externalSuffix <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent field parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         < x = ( |
            | 
            name < x name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         = x = ( |
            | 
            name = x name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         childResponsibilities = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> 'childResponsibilities' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent field parent childResponsibilities.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         copyName: n Bits: bits Asm: myAsm = ( |
            | 
            (copy name: n canonicalize)  where:  myAsm bitRange bits: bits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         hash = ( |
            | 
            where hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         intNN = ( |
            | where intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         isConstantField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         isIgnoredField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         isOperandField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         isOptionField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         isReservedField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         printString = ( |
            | 
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredIllegalTestCasesDo: blk = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredLegalTestCases = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'field' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (assemblerSystems framework bitRange)'
        
         where <- bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: option fields\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         optionField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'optionField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent field copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'optionField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent optionField.

CopyDowns:
globals assemblerSystems framework generators fields parent field. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: name spaces\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         instructionAssemblyMethods = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'instructionAssemblyMethods' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework instructionAssemblyMethods.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: name spaces\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         instructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'instructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework instructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         instructionTemplatesDo: blk = ( |
            | 
            (reflect: instructionTemplates) do: [|:s|
              blk value: s contents reflectee With: s name].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         intNN = ( |
            | int32).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         intNNFromBytes: b = ( |
            | intNNFromBytes: b At: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         intNNFromBytes: instBytes At: offset = ( |
            | 
            intNNFromBytes: instBytes At: offset IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         isLittleEndian = ( |
            | isBigEndian not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: enndianness and word size\x7fCategory: mixins - children should inherit from either of these\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         littleEndianMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework littleEndianMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVectorFromInt: int = ( |
            | 
            [endianDependentServer].
            intNN asLittleEndianByteVectorFrom: int).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFromInt: anInt Do: blk = ( |
            | 
            [endianDependentServer].
            intNN littleEndianBytesFrom: anInt Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         int32VectorFromBytes: bytes = ( |
            | 
            bytes asLittleEndianInt32Vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         intNNFromBytes: instBytes At: offset IfFail: errBlk = ( |
            | 
            intNN copyTakeLittleEndianBytesFrom: instBytes Index: offset IfFail: errBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         isBigEndian = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         store: aWord AsByteVectorInto: bytes At: i IfFail: fb = ( |
            | 
            aWord storeAsLittleEndianByteVectorInto: bytes At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         loggingAssembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals loggingSender copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework loggingAssembler.

CopyDowns:
globals loggingSender. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')'
        
         logCollector <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework loggingAssembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOrigin: o = ( |
            | 
            copyForTarget: myAssemblerSystem assembler copyOrigin: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         locationCounterString = ( |
            | 
            startLC hexPrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         log = ( |
            | 
            logCollector flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         log: when Message: aMessage = ( |
            | 
            logEntry:  when, ': ', aMessage statePrintStringWithoutReceiver).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         logEntry: s = ( |
            | 
            logCollector: logCollector & s & '\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         logFinish: aMessage = ( |
            | 
            startLC = receiver locationCounter
              ifFalse: [log: locationCounterString Message: aMessage].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         logStart: aMessage = ( |
            | 
            startLC: receiver locationCounter.
            aMessage selector = 'comment:' ifTrue: [
              logEntry: '** ', aMessage arguments first, ' **'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'loggingSender' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         send: aMsg = ( |
            | 
            aMsg selector = 'log'
              ifTrue: [log]
               False: [resend.send: aMsg]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         startLC <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: name spaces\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         pseudoInstructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'pseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework pseudoInstructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         pseudoInstructionTemplatesDo: blk = ( |
            | 
            (reflect: pseudoInstructionTemplates) do: [|:s|
              blk value: s contents reflectee With: s name].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         realAndPseudoInstructionTemplatesDo: blk = ( |
            | 
            instructionTemplatesDo: blk.
            pseudoInstructionTemplatesDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fCategory: registers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         registerNameSpaces = ( |
             r.
            | 
            r: list copyRemoveAll.
            registerNameSpacesDo: [|:ns| r add: ns].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fCategory: registers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         registerNameSpacesDo: blk = ( |
            | 
            blk value: registers).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: name spaces\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         registers = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'registers' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework registers.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fCategory: registers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         registersForNameSpace: ns = ( |
            | 
            (((reflect: ns) asList 
              copyFilteredBy: [|:s| s isParent not && [s isAssignment not]]) 
              copyMappedBy: [|:s| s contents reflectee]) asVector
              sortBy: sortRegistersByNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: iterating\x7fCategory: registers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         sortRegistersByNumber = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'sortRegistersByNumber' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework sortRegistersByNumber.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'sortRegistersByNumber' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         element: r1 Precedes: r2 = ( |
            | r1 number < r2 number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'sortRegistersByNumber' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForConditionCodeValue: n = ( |
            | 
            "could be just '16r', n hexPrintString"
            '<<stringForConditionCodeValue:>> needs to be implemented in: <<', asMirror name, '>>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         test = ( |
            | 
            tester copy testAsm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         testAll = ( |
            | tester testAsm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         tester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework tester.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)'
        
         badInsts.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         binaryPos <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)'
        
         goodInsts.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myOpCases.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework tester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmArgs = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: file names\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         asmBadTestFileName = ( |
            | 
            asmFileNamePrefix, 'Bad.s').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmBaddiesCommand = ( |
            | 
            'rm -f ', errorFileName, '; ',
            'cc ', asmArgs, ' ', asmBadTestFileName, ' >& ', errorFileName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmBaddiesSource = ( |
            | 
            asmFileSourceFor: badInsts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: file names\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         asmFileNamePrefix = 'testAsm'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmFilePrefix = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmFilePrefixLineCount = ( |
            | asmFilePrefix asTextLines height pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmFileSourceFor: insts = ( |
             r <- ''.
            | 
            r: asmFilePrefix.
            insts do: [|:i|
              r: r & (externalSourceFor: i) & '\n'.
            ].
            r: r & asmFileSuffix.
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmFileSuffix = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: file names\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         asmGoodTestFileName = ( |
            | 
            asmFileNamePrefix, 'Good.s').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         asmGoodiesSource = ( |
            | 
            asmFileSourceFor: goodInsts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: file names\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         binaryFileName = ( |
            | 
            asmFileNamePrefix, '.out').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         buildBadOnes = ( |
             ocov <- 0.
             target = 0.8.
            | 
            'building bad ones:' printLine.
            [
              ocov: myOpCases illegalCoverage.
              myAssemblerSystem realAndPseudoInstructionTemplatesDo: [|:it.  |
                it name printLine.
                it isExternallyTestableForFailure ifTrue: [|opsList|
                  opsList: list copyRemoveAll.
                  myOpCases illegalTupleAndCommentFor: it
                                             IfExists: [|:ops. :cmt| opsList addLast: ops @ cmt].
                  opsList addAll: it illegalTests copyMappedBy: [|:t| t @ 'exemplar from spec'].
                  badInsts addAll: opsList copyMappedBy: [|:opAndCmt| 
                    instructionTester copyForAssemblerSystem: myAssemblerSystem
                                                    Template: it
                                            RelativeOperands: opAndCmt x
                                                     Comment: opAndCmt y
                  ].
                ].
              ].
            ] untilTrue: [|ncov|
                ncov: myOpCases illegalCoverage.
                    (((ncov - ocov) * 10) < target)
                || [ncov >= target]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         buildGoodOnes = ( |
             a.
            | 
            "Some instructions have restrictions on operands relative
             to each other. So if assembler fails, add inst to baddies.
             Retry till you get enough good ones per template."

            'building goodies:' printLine.
            a: myAssemblerSystem assembler copy.
            myAssemblerSystem realAndPseudoInstructionTemplatesDo: [|:it.|
              it name printLine.
              it isExternallyTestableForSuccess ifTrue: [
                (myOpCases legalTuplesForIT: it ) do: [
                  |:aTuple. i. | 
                  i: instructionTester
                       copyForAssemblerSystem: myAssemblerSystem
                                     Template: it
                             RelativeOperands: aTuple
                                      Comment: 'good operands'.
                  i assembleWith: a IfFail: [|:why|
                    it isExternallyTestableForFailure  ifTrue: [
                      i comment: 'failed: ', why.
                      badInsts addLast: i
                    ].
                    i: nil
                  ].
                  i ifNotNil: [goodInsts addLast: i].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         buildInstructionsToTest = ( |
            | 
            "Some instructions have restrictions on operands relative
             to each other. So if assembler fails, add inst to baddies.
             Retry till you get a good one per template."

            goodInsts: list copyRemoveAll.
             badInsts: list copyRemoveAll.

            userQuery show: 'Building legal test cases...' 
                      While: [buildGoodOnes].
            userQuery show: 'Building illegal test cases...'
                      While: [buildBadOnes].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building operand case generator\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         buildOperandTestCaseGenerator = ( |
            | 
            myOpCases: operandTestCaseGenerator copyForAssemblerSystem: myAssemblerSystem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: file names\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         cTestFileName = ( |
            | 
            asmFileNamePrefix, '.c').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         cTestFileSource = ( |
            | 
            '# include <fcntl.h>

            extern char *', startSym, ', *', endSym, ';
            extern void exit(int);

            main() {
              int f;
              int len;
              f = open("', binaryFileName, '", O_WRONLY | O_CREAT | O_TRUNC, 0666);
              if (f < 0) {
                perror("could not open file");
                exit(1);
              }
              len = (int)&', endSym, ' - (int)&', startSym, ';
              if (write(f, &', startSym, ', len) !=  len) {
                perror("write failed");
                exit(1);
              }
              close(f);
              exit(0);
            }\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         checkExternalGoodiesResult = ( |
             bin.
             lineNo <- 0.
            | 
            bin: readBinary.
            binaryPos: 0.
            lineNo: asmFilePrefixLineCount succ.
            goodInsts do: [|:i. |
              i checkAgainstExternalBinary: (extractInstructionFor: i From: bin)
                                      Line: lineNo.
              lineNo: lineNo succ.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         checkForErrors = ( |
             errDelta.
             errSet.
             errs.
            | 
            errs: errorFileName asFileContents.
            errSet: set copyRemoveAll.

            errs asTextLines do: [|:line. toks|
              extractLineNumberFrom: line Into: [|:n| errSet add: n].
            ].
            errDelta: asmFilePrefixLineCount succ. "succ for 0-origin"
            badInsts asVector do: [|:inst. :i. lineNo|
              lineNo: i + errDelta.
              (errSet includes: lineNo)  ifFalse: [
                error: 'Line ', lineNo  printString, ' should have failed because: ', inst comment
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         clearExternalFiles = ( |
            | 
             exec:  'rm ', asmFileNamePrefix, '*'
            IfFail:  []).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         compileAndRunCommand = ( |
             command.
            | 
            command: 'rm -f a.out; cc ', asmArgs, ' ', cTestFileName, ' ', asmGoodTestFileName, '&& ./a.out'.
            command printLine.
            command).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: utilities\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         copyText: t = ( |
            | 
            (ui2_textBuffer contents: t) contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembleAndCheckGoodies = ( |
            | 
            goodInsts do: [|:i| i checkDisassembler].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         doTests = ( |
            | 
            userQuery show: 'Building test case generator..'
                     While: [buildOperandTestCaseGenerator].
            buildInstructionsToTest.

            clearExternalFiles.
            testGoodOnes.
            testBadOnes.

            reportOperandCoverage).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         endSym = 'endAsm'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: file names\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         errorFileName = ( |
            | 
            asmFileNamePrefix, 'Errors').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: utilities\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         exec: cmd IfFail: failBlock = ( |
             r.
            | 
            r: os command: cmd
               IfFail: [ userQuery report: 'Please do this: \n',
                              (copyText: cmd).
                       ^ self
               ].
            r = 0 ifFalse: [failBlock value: r] True: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         externalSourceFor: instructionTester = ( |
            | instructionTester externalSource).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         externallyAssembleBaddies = ( |
            | 
            generateBaddiesFile.
            asmBaddiesCommand printLine.
            exec: asmBaddiesCommand IfFail: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         externallyAssembleGoodies = ( |
            | 
            generateGoodiesFiles.
            exec: compileAndRunCommand 
            IfFail: [ error: 'Compilation of good cases failed; check console window for error messages' ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         extractInstructionFor: i From: bin = ( |
             end.
             r.
            | 
            end: binaryPos + i instructionSize.
            r: (myAssemblerSystem int32VectorFromBytes: bin copyFrom: binaryPos UpTo: end) first.
            binaryPos: end.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         extractLineNumberFrom: line Into: block = ( |
             tokens.
            | 
            tokens: line asTokensSeparatedByCharactersIn: ':'.
            tokens isEmpty not && [tokens first = asmBadTestFileName] ifTrue: [
                block value: ((tokens at: 1) asIntegerIfFail: 0).
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         generateBaddiesFile = ( |
            | 
            asmBadTestFileName setFileContentsTo: asmBaddiesSource.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         generateGoodiesFiles = ( |
            | 
                  cTestFileName setFileContentsTo: cTestFileSource.
            asmGoodTestFileName setFileContentsTo:   asmGoodiesSource.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         instructionTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework tester parent instructionTester.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         absoluteOperands <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         comment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         disassembly <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         internalBinary <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         locationCounter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myAssemblerSystem.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myInstructionTemplate.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework tester parent instructionTester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: assembling internally\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         absoluteOperandValues = ( |
            | 
            absoluteOperands copyMappedBy: [|:op| op value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: assembling internally\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assembleWith: asm = ( |
            | 
            assembleWith: asm IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: assembling internally\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         assembleWith: asm IfFail: failBlk = ( |
             di.
            | 
            locationCounter: asm locationCounter.
            absoluteOperands: list copyRemoveAll.
            relativeOperands with: instructionTemplate operandFields  Do: [
              |:op. :field| 
              absoluteOperands addLast:
                field unadjust: op IfRelativeAt: locationCounter IT: instructionTemplate.
            ].
            di: myAssemblerSystem disassembledInstruction
                  copyForInstructionTemplate: instructionTemplate
                                    Operands: absoluteOperands
                                       Start: locationCounter.
            asm failBlock: [|:e| ^ failBlk  value: e].
            internalBinary: asm reassemble: di.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: comparing internal vs. external\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         checkAgainstExternalBinary: eb Line: lineNo = ( |
            | 
            (isSameAsExternalBinary: eb)
             ifFalse: [ reportMismatchExternalBinary: eb Line: lineNo].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: testing disassembler\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         checkDisassembler = ( |
            | 
            instructionTemplate isPseudo ifTrue: [^ self].
            checkDisassemblerSpecificMode: true.
            checkDisassemblerSpecificMode: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: testing disassembler\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         checkDisassemblerSpecificMode: sm = ( |
             a.
             di.
             reassemblyResult.
            | 
            di: disassembledInstructionForSpecificMode: sm.
                  di isRealInstruction
             && [(di instructionTemplate = instructionTemplate)
             && [ di operands = absoluteOperandValues]]
              ifTrue: [^ self].

            "Since two insts may assemble to same binary (if one is shorthand)
             disassemble, then reassemble and change binaries -- dmu 1/2"
            a: myAssemblerSystem assembler copyOrigin: locationCounter.
            reassemblyResult: a reassemble: di.
            internalBinary = reassemblyResult  ifFalse: [
              reportDisassemblerErrorDI: di Result: reassemblyResult
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForAssemblerSystem: a Template: it RelativeOperands: ops Comment: c = ( |
            | 
            (((copy
            myAssemblerSystem: a) 
            myInstructionTemplate: it)
            relativeOperands: ops)
            comment: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: testing disassembler\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledInstructionForSpecificMode: sm = ( |
             d.
            | 
            d: myAssemblerSystem disassembler copyOrigin: locationCounter.
            sm ifTrue: [d beSpecific] False: [d beGeneral].
            d disassemble: internalBinary).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: comparing internal vs. external\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         externalBitsToTest = ( |
            | 
            instructionTemplate externalBitsToTest).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: assembling externally\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         externalSource = ( |
             di.
             ofs.
             r.
             sp <- '    '.
            | 
            locationCounter ifNil: [
              error: 'must have assembled me first'.
            ].
            di: myAssemblerSystem disassembledInstruction
              copyForInstructionTemplate: instructionTemplate
                                Operands: absoluteOperands
                             Instruction: internalBinary
                                   Start: locationCounter
                                     End: locationCounter + myInstructionTemplate size.
            di externalSourceShowingLCOffset: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: should really be somewhere else\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         instructionSize = ( |
            | 
            "what about var-length insts?"
            intNN size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         instructionTemplate = ( |
            | myInstructionTemplate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: should really be somewhere else\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            myAssemblerSystem intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: comparing internal vs. external\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         isSameAsExternalBinary: eb = ( |
            | 
            intNN eq:   (intNN and: internalBinary With: externalBitsToTest)
                  With: (intNN and: eb             With: externalBitsToTest)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: testing disassembler\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         reportDisassemblerErrorDI: di Result: reassemblyResult = ( |
            | 
            error: 'instruction: ', instructionTemplate name, '\n',
                   'relative operands: ', relativeOperands printString, '\n',
                   'absolute operands: ', absoluteOperands printString, '\n',
                   'internal binary: ', internalBinary printString, '\n',
                   'disassembled to: ', di instructionTemplate name, '\n',
                   'absolute operands: ', di operands printString, '\n',
                   'which assembled to: ', reassemblyResult printString, '\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'Category: comparing internal vs. external\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         reportMismatchExternalBinary: eb Line: lineNo = ( |
            | 
            error: 'instruction: ', instructionTemplate name, ' mismatch\n',
                   'line: ', lineNo printString, '\n',
                   'externalSource: ', externalSource, '\n',
                   'relative operands: ', relativeOperands printString, '\n',
                   'absolute operands: ', absoluteOperands printString, '\n',
                   'internal binary: ', internalBinary printString, '\n',
                   'external binary: ', eb printString, '\n',
                   'internal disasm: ', ((myAssemblerSystem disassembler copyOrigin: locationCounter) disassembleSourceIn: internalBinary), '\n',
                   'external disasm: ', ((myAssemblerSystem disassembler copyOrigin: locationCounter) disassembleSourceIn: eb).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         relativeOperands <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         internallyAssembleGoodies = ( |
             a.
            | 
            a: myAssemblerSystem assembler copy.
            goodInsts do: [|:i| i assembleWith: a].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building operand case generator\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         operandTestCaseGenerator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework tester parent operandTestCaseGenerator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: operand cases summary\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         baddies <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: dynamic state\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         fields <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: operand cases summary\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         goodies <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: coverage measurment\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         illegalCasesTested <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: dynamic state\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         index <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: coverage measurment\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         legalCasesTested <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem <- bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework tester parent operandTestCaseGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: creation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((resend.copy
              goodies: goodies copy)
              baddies: baddies copy)
              fields:  fields  copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: creation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForAssemblerSystem: myA = ( |
            | 
            (copy myAssemblerSystem: myA) queryOperandFields).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: coverage\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         coverageString = ( |
            | 
            'Legal operand coverage = ', (legalCoverage * 100) asInteger printString, '%.\n',
            'Illegal operand coverage = ', (illegalCoverage * 100) asInteger printString, '%.\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: coverage\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         illegalCoverage = ( |
            | 
            illegalCasesTested asFloat / (1 max: totalIllegalCases)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: generation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         illegalTupleAndCommentFor: it IfExists: blk = ( |
             badOne.
             fields.
             illTuple.
             whichOperandField <- 0.
            | 
            fields: it operandFields.
            fields isEmpty ifTrue: [^ self].
            whichOperandField: index % fields size.
            badOne: fields asVector at: whichOperandField.
            index: index succ.
            illTuple:
              fields asVector copyMappedBy: [|:f|
                nextTestCaseFor: f
                 From:
                  ( badOne = f ifTrue: [baddies]
                                False: [goodies])
                 IfNone: [^ self].
              ].
            blk value: illTuple 
                 With: 'illegal operand for field ', badOne name, ' (#', whichOperandField succ printString, ')').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: coverage\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         legalCoverage = ( |
            | legalCasesTested asFloat / totalLegalCases).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: generation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         legalTuplesForIT: it = ( |
            | 
            (legalTuplesWithZeroesForIT: it)
              addLast: randomLegalTupleForIT: it).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: generation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         legalTuplesWithZeroesForIT: it = ( |
             fields.
             r.
            | 
            fields: it operandFields.
            r: list copyRemoveAll.
            fields asVector do: [|:f. :i|
              f legalTestCases
                findFirst: [|:t| t value = 0]
                IfPresent: [|:op|
                  r addLast: (randomLegalTupleForIT: it) 
                               at:  i
                               Put: op
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: generation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         nextTestCaseFor: f From: dict IfNone: noneBlk = ( |
             gs.
             r.
            | 
            gs: dict at: f.
            gs isEmpty ifTrue: [^ noneBlk value].
            r: gs at: index % gs size.
            index: index + f hash.
            dict = goodies ifTrue: [  legalCasesTested:   legalCasesTested succ]
                            False: [illegalCasesTested: illegalCasesTested succ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: setup\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         queryOperandFields = ( |
            | 
            goodies: goodies copyRemoveAll.
            baddies: baddies copyRemoveAll.
            fields:  fields  copyRemoveAll.
            legalCasesTested: 0.
            illegalCasesTested: 0.
            totalLegalCases: 0.
            totalIllegalCases: 0.
            index: 0.
            myAssemblerSystem fieldsDo: [|:f. g. b|
              f isOperandField ifTrue: [
                fields addLast: f.
                g: f   legalTestCases asVector.
                b: f illegalTestCases asVector.
                goodies at: f Put: g.
                baddies at: f Put: b.
                totalLegalCases:   totalLegalCases   + g size.
                totalIllegalCases: totalIllegalCases + b size.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> 'parent' -> () From: ( | {
         'Category: generation\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         randomLegalTupleForIT: it = ( |
            | 
            it operandFields asVector copyMappedBy: [|:f|
              nextTestCaseFor: f From: goodies IfNone: [error: 'no test case']
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: coverage measurment\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         totalIllegalCases <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'operandTestCaseGenerator' -> () From: ( | {
         'Category: coverage measurment\x7fModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (0)'
        
         totalLegalCases <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         readBinary = ( |
            | binaryFileName asFileContents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: summarizing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         reportOperandCoverage = ( |
            | 
            userQuery reportAndContinue:
              myOpCases coverageString, '\n',
              'Good cases: ', goodInsts size printString, '.\n',
              'Bad cases: ',   badInsts size printString, '.\n',
              'Generated assembler methods: ',
              (reflect: myAssemblerSystem instructionAssemblyMethods) size printString, '.').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         startSym = 'startAsm'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         testAsm = ( |
            | 
            copy doTests).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         testBadOnes = ( |
            | 
            userQuery show: 'Ensuring internal assembler rejects illegal ones...' While: [testBadOnesInternally].
            userQuery show: 'Ensuring external assembler rejects illegal ones...' While: [testBadOnesExternally].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         testBadOnesExternally = ( |
            | 
            externallyAssembleBaddies.
            checkForErrors).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         testBadOnesInternally = ( |
             a.
            | 
            a: myAssemblerSystem assembler copy.
            badInsts do: [|:i. failed <- false|
             i assembleWith: a IfFail: [failed: true].
             failed ifFalse: [error: 'should have failed'].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         testGoodOnes = ( |
            | 
            userQuery show: 'Internally assembling legal ones...'        While: [internallyAssembleGoodies].
            userQuery show: 'Externally assembling legal ones...'        While: [externallyAssembleGoodies].
            userQuery show: 'Comparing internal to external binaries...' While: [checkExternalGoodiesResult].
            userQuery show: 'Disassembling and checking legal ones...'   While: [disassembleAndCheckGoodies].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         asmFrame = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrame.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.52 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame' -> () From: ( | {
         'ModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrame2
asmBitRange1
asmFrameAbsGen
asmSignOps
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'integer' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: asmFrame InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAssemblerExternalStoreString = ( |
            | decimalPrintString).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrame2' From: 'applications/asmKit/asmFrame'
 bootstrap read: 'asmBitRange1' From: 'applications/asmKit/asmFrame'
 bootstrap read: 'asmFrameAbsGen' From: 'applications/asmKit/asmFrame'
 bootstrap read: 'asmSignOps' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmFrame postFileIn
