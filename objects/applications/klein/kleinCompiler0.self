 '$Revision: 30.20 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fCategory: compiler 0\x7fComment: Old & unused. -- dmu 6/03\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         compiler0 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler0.

CopyDowns:
globals abstractBytecodeInterpreter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (vector)'
        
         bytecodeLabels <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (nil)'
        
         gen.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (vector)'
        
         localRegisters <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (0)'
        
         nextStackRegister <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler0 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         accessLocal: bc = ( |
             r.
            | 
            bc lexicalLevel = 0 ifFalse: [unimplemented].
            [todo compiler0]."need to init locals"
            r: localRegisters at: bc indexOfLocal. [todo compiler0]. "locals overflow"
            bc isWrite ifTrue: [ 
              gen genMoveFrom: topStackRegister To: r.
              freeTopStackRegister.
              pushSelf: bc. "writeLocal: simulates msg that returns self"
            ]
            False: [
              gen genMoveFrom: r To: allocateNextStackRegister
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: relocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocator: or = ( |
            | 
            relocators addLast: or.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         allocateNextStackRegister = ( |
            | 
            nextStackRegister: nextStackRegister pred.
            topStackRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         allocateTemporaryRegister = ( |
            | 
            temporaryRegisters removeFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: compatibilitywith compiler1\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         architecture = 'ppc'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: compatibilitywith compiler1\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         architecture: x = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: method start instruction sentinel\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (nil)'
        
         cachedMethodStartInstruction.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         compileMethod: m Architecture: betterBePPC = ( |
            | 
            copyInterpretMethod: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         compileReceiver: r Slot: s Architecture: ppcOnly Debug: ignored = ( |
            | 
            ppcOnly = 'ppc' ifFalse: [
              error: 'unimplemented'
            ].
            copyInterpretMethod: s contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: labels for branches\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         defineBytecodeLabel = ( |
            | 
            gen bindLabel:  bytecodeLabels at: pc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: debugging\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         disassemble = ( |
            | 
            assemblerSystems ppc disassembler copy beSpecific disassembleAllExternalSource: gen uncheckedAssembledBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         doPrimitive: sel = ( |
            | 
            [generatePrimitive_IntAdd_]. "Browsing"
            'generatePrimitive', (sel copyMutable mapBy: [|:c| c = ':' ifTrue: '_' False: c])
              sendTo: gen).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         doSend: sel = ( |
            | 
            sel first = '_'  ifTrue: [^ doPrimitive: sel].
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         freeTemporaryRegister: r = ( |
            | 
            temporaryRegisters addFirst: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         freeTopStackRegister = ( |
             r.
            | 
            r: topStackRegister.
            nextStackRegister: nextStackRegister succ.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: relocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         ifOop: oop IsImmediateThenDoWithAddress: blk = ( |
            | 
            klein layouts object
              if: oop IsImmediateThenDoWithEncoding: blk Else: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         indexedBranch: bc = ( |
            | 
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: labels for branches\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         initBytecodeLabels = ( |
            | 
            bytecodeLabels: bytecodeLabels copySize: codes size.
            pc upTo: codes size Do: [|:i|
              bytecodeLabels at: i Put: gen newLabel
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         initRegisters = ( |
            | 
            temporaryRegisters: (gen r31 & gen r30) asList.
            localRegisters: (gen r29 & gen r28 & gen r27 & gen r26) asVector.
            nextStackRegister: 26).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            relocators: relocators copyRemoveAll.
            gen: ppcCodeGenerator copyForCompiler: self.
            initBytecodeLabels.
            initRegisters.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting general\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretBytecode = ( |
            | 
            defineBytecodeLabel.
            resend.interpretBytecode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting general\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretMethod = ( |
            | 
            gen generateMethodStartInstruction.
            resend.interpretMethod.
            gen trap.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: labels for branches\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         labelForBytecode: pc = ( |
            | 
            bytecodeLabels at: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting general\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         machineCode = ( |
            | 
            gen assembledBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: method start instruction sentinel\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         methodStartInstruction = ( |
            | 
            cachedMethodStartInstruction ifNil: [
              cachedMethodStartInstruction: ppcCodeGenerator copy generateMethodStartInstruction.
            ].
            cachedMethodStartInstruction).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         nonlocalReturn: bc = ( |
            | 
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         pop: bc = ( |
            | 
            freeTopStackRegister.
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         ppcCodeGenerator = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems ppc assembler copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler0 parent ppcCodeGenerator.

CopyDowns:
globals assemblerSystems ppc assembler. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (nil)'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler0 parent ppcCodeGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            copy compiler: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: machine-indep\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         genBranchTo: d = ( |
            | 
            bDisp: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: machine-indep\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         genBranchTo: d IfRegister: r EqualsOop: oop = ( |
             t.
            | 
            t: compiler allocateTemporaryRegister.
            loadOopTo: t From: oop.
            cmpwFrom: r With: t.
            compiler freeTemporaryRegister: t.
            beqDisp: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: machine-indep\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         genMoveFrom: rs To: rd = ( |
            | 
            mrTo: rd From: rs.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: machine-indep\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMethodStartInstruction = ( |
            | 
            bDisp: locationCounter + intNN size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_Breakpoint = ( |
            | 
            trap.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_Eq_ = ( |
             join.
             notEq.
             r1.
             r2.
             rd.
            | 
            r2: compiler freeTopStackRegister.
            r1: compiler freeTopStackRegister.

            cmpwFrom: r1 With: r2.
            notEq: newLabel.
            bneDisp: notEq.

            rd: compiler allocateNextStackRegister.
            loadOopTo: rd From: true.
            join: newLabel.
            bDisp: join.

            bindLabel: notEq.
            loadOopTo: rd From: false.

            bindLabel: join.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_IntAdd_ = ( |
            | 
            generateSimpleBinaryPrimitiveWith: 
             [|:rd. :r1. :r2|
              addTo: rd From: r1 With: r2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_IntMul_ = ( |
             r2.
            | 
            srawiTo: compiler topStackRegister From: compiler topStackRegister By: klein tag size. 
            r2: compiler freeTopStackRegister.
            mullwTo: compiler topStackRegister From: compiler topStackRegister With: r2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSimpleBinaryPrimitiveWith: blk = ( |
             r1.
             r2.
             rd.
            | 
            r2: compiler freeTopStackRegister.
            r1: compiler     topStackRegister.
            blk value: r1 With: r1 With: r2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'Category: ppc\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         loadOopTo: reg From: oop = ( |
             or.
            | 
            compiler ifOop: oop IsImmediateThenDoWithAddress: [|:a|
             ^ load32To: reg From: a
            ].
            or: klein relocators loadAddress
                  copyDstReg: reg Offset: locationCounter Oop: oop.
            compiler addRelocator: or.
            or assembleRealOrPlaceholderInstructionsWith: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> 'ppcCodeGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         pushLiteral: bc = ( |
            | 
            gen    loadOopTo: allocateNextStackRegister
                       From:  bc oopToPush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         pushSelf: bc = ( |
            | 
            gen genMoveFrom: selfRegister To: allocateNextStackRegister.
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         scalarBranch: bc = ( |
             d.
            | 
            d: labelForBytecode: bc destination.
            bc isConditional ifFalse: [
              gen genBranchTo: d
            ]
            True: [ 
              gen genBranchTo: d
                   IfRegister: topStackRegister
                    EqualsOop: bc valueToBranchOn.
              freeTopStackRegister
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         selfRegister = ( |
            | 
            [todo compiler0]. "ppc"
            gen r3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         send: bc = ( |
            | 
            doSend: bc selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> 'parent' -> () From: ( | {
         'Category: register allocation\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         topStackRegister = ( |
            | 
            assemblerSystems ppc operands 
             gprFor: nextStackRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         relocators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler0' -> () From: ( | {
         'Category: compiler0\x7fModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         temporaryRegisters <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: used only by old compiler\x7fModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         if: obj IsImmediateThenDoWithEncoding: immBlk Else: elseBlk = ( |
             lo.
            | 
            "cannot use layoutOf: because might get an unmapped object"
            [todo cleanup compiler0]. "still needed now that we don't use compiler0?"
            lo: case if: [isSmi:   obj] Then: layouts smi
                     If: [isFloat: obj] Then: layouts float
                                        Else: [^ elseBlk value].
            immBlk value: lo encode: obj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         kleinCompiler0 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinCompiler0.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.20 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler0' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler0 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinCompiler0 postFileIn
