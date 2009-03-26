 '$Revision: 30.32 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         ppc = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         assembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework assembler copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc assembler.

CopyDowns:
globals assemblerSystems framework assembler. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc assembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: adding/subractng\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         add32To: dstReg From: srcReg With: imm = ( |
            | 
            ifWord: imm FitsInSIThen: [
              addiTo: dstReg From: srcReg With: imm
            ]
            Else: [
              [todo untested].
              untested.
              breakUpWord: imm ForAddingAndDo: [|:low16Bits. :high16Bits|
                addisTo: dstReg From: srcReg With: high16Bits.
                addiTo:  dstReg From: dstReg With: low16Bits.
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: branches\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         branch32ViaCTRTo: dest UsingTemp: reg SetLink: shouldSetLink = ( |
            | 
            load32To: reg From: dest.
            mtctrFrom: reg.
            shouldSetLink ifTrue: [bctrl] False: [bctr]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: branches\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         branch32ViaLRTo: dest UsingTemp: reg = ( |
            | 
            load32To: reg From: dest.
            mtlrFrom: reg.
            blr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToLabel: lbl = ( |
            | 
            bDisp: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: decomposing 32-bit words\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         breakUpWord: w ForAddingAndDo: blk = ( |
             hiExtra <- 0.
             high16Bits.
             loExtra <- 0.
             low16Bits.
            | 
            (int32 and: w With: 16r8000) != 0  ifTrue: [
              hiExtra: 1.  loExtra: -16r10000.
            ].
            low16Bits:   int32 and: w With: 16rffff.
            high16Bits:  int32 shr: w With: 16.
            blk value:  (int32 add: low16Bits  With: loExtra)
                 With:  (int32 add: high16Bits With: hiExtra)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: decomposing 32-bit words\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         breakUpWord: w ForOringAndDo: blk = ( |
            | 
            blk value:  (int32  and: w With: 16rffff)
                 With:  (int32 ushr: w With: 16)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         commentSentinel = ( |
            | 
            twneFrom: r0 With: r0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         fields = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc fields.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         fields* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: decomposing 32-bit words\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         ifWord: w FitsInSIThen: okBlk Else: failBlk = ( |
            | 
            (si where isSignedIntegerInRange: w) ifTrue: okBlk False: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         instructionAssemblyMethods = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'instructionAssemblyMethods' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc instructionAssemblyMethods.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         instructionAssemblyMethods* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'instructionAssemblyMethods' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: loading & storing\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         load32To: reg From: imm = ( |
            | 
            ifWord: imm FitsInSIThen: [
              liTo: reg With: imm
            ]
            Else: [
              breakUpWord: imm ForOringAndDo: [|:low16Bits. :high16Bits|
                lisTo: reg With: high16Bits.
                oriTo: reg From: reg With: low16Bits.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: loading & storing\x7fComment: Loads immediate just like a load or store sequence.
Used for testing.\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         loadAddressTo: baseReg From: addr = ( |
            | 
            breakUpWord: addr ForOringAndDo: [|:low16Bits. :high16Bits|
              lisTo: baseReg With: high16Bits.
              oriTo: baseReg From: baseReg With: low16Bits
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: loading & storing\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         loadWordTo: dstReg FromAddress: addr UsingBase: baseReg = ( |
            | 
            breakUpWord: addr ForAddingAndDo: [|:low16Bits. :high16Bits|
              lisTo: baseReg With: high16Bits.
              lwzTo:  dstReg Disp: low16Bits Base: baseReg
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         operands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc operands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         operands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: useful macros\x7fCategory: loading & storing\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         storeWordFrom: srcReg ToAddress: addr UsingBase: baseReg = ( |
            | 
            breakUpWord: disp ForAddingAndDo: [|:low16Bits. :high16Bits|
              lisTo: baseReg With: high16Bits.
              stwFrom: srcReg Disp: low16Bits Base: baseReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fComment: Or these masks together for the \"to\" or \"tos\" field of a trap instruction.\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         toMasks = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc toMasks.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Comment: Or these masks together for the \"to\" or \"tos\" field of a trap instruction.\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         toMasks* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: resolving clashes between fields and global names\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         ui = ( |
            | 
            fields.ui).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         bigEndianMixin* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: caches\x7fModuleInfo: Module: asmPPC InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllRegisterLocations.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: caches\x7fModuleInfo: Module: asmPPC InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllRegisters.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         conditionCodeRegisterName = ( |
            | 'cr').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         crBits = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crBits' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc crBits.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crBits' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         eq = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crBits' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         gt = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crBits' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         lt = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crBits' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         so = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crBits' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         un = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         crFields = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crFields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc crFields.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledInstruction = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembledInstruction' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework disassembledInstruction copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembledInstruction' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc disassembledInstruction.

CopyDowns:
globals assemblerSystems framework disassembledInstruction. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembledInstruction' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc disassembledInstruction parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         externalOpcode = ( |
             n.
            | 
            n: resend.externalOpcode copyMutable copyMappedBy: [|:c| c = '_' ifTrue: '.' False: c].
            case if: [  'Taken' isSuffixOf: n] Then: [(n copySize: n size -   'Taken' size), '+']
                 If: ['Untaken' isSuffixOf: n] Then: [(n copySize: n size - 'Untaken' size), '-']
                                               Else: [n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework disassembler copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc disassembler.

CopyDowns:
globals assemblerSystems framework disassembler. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc disassembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'disassembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForConditionCodeValue: n = ( |
             cr0.
             r <- ''.
            | 
            "Just cr0 value for now--dmu 3/02."
            "See Page 33 of the PPC book."
            cr0: n >> 28.
            r: r, ((cr0 && 1)  asBoolean   ifTrue: ' SO' False: '').
            r: r, ((cr0 && 2)  asBoolean   ifTrue: ' EQ' False: '').
            r: r, ((cr0 && 4)  asBoolean   ifTrue: ' GT' False: '').
            r: r, ((cr0 && 8)  asBoolean   ifTrue: ' LT' False: '').
            r isEmpty ifTrue: '' False: [r copyWithoutFirst]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         fprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc fprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         generators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionTemplates parent proto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         architecture = 'ppc'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         gprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'gprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc gprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         instructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'instructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc instructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         loggingAssembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'loggingAssembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework loggingAssembler _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'loggingAssembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc loggingAssembler.

CopyDowns:
globals assemblerSystems framework loggingAssembler. _Clone 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'loggingAssembler' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'loggingAssembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc loggingAssembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         crFields* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crFields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         fprs* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         gprFor: n = ( |
            | 
            [r0.  sp. rtoc. r3.  r4.  r5.  r6.  r7.  r8.  r9.
             r10. r11. r12. r13. r14. r15. r16. r17. r18. r19.
             r20. r21. r22. r23. r24. r25. r26. r27. r28. r29.
             r30. r31]. "browsing"

            0 case if: [n = 1] Then: [sp]
                   If: [n = 2] Then: [rtoc]
                               Else: ['r', n asString sendTo: self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         gprNumberFor: regName IfPresent: gprBlk IfAbsent: absentBlk = ( |
            | 
            regName isEmpty                       ifTrue: [^ absentBlk value: 'name is empty'].
            regName       = 'sp'                  ifTrue: [^    gprBlk value: 1      ].
            regName       = 'rtoc'                ifTrue: [^    gprBlk value: 2      ].
            regName first = 'r'                   ifTrue: [     gprBlk value: regName copyWithoutFirst asInteger ]
                                                   False: [  absentBlk value: regName, ' does not exist' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         gprs* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'gprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         sprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'sprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc sprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         sprs* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'sprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: name spaces of parts of the architecture\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         pseudoInstructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'pseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc pseudoInstructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         registerNameSpacesDo: blk = ( |
            | 
            "until debuggers for fprs & sprs, just do gprs--dmu 1/02"
            blk value: gprs.
            "
              blk value: fprs.
              blk value: sprs
            ").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         testLabels = ( |
            | 
            assembler copy generateTri).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         tester = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework tester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc tester.

CopyDowns:
globals assemblerSystems framework tester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc tester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing good instructions\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         asmArgs = ( |
             r <- ''.
            | 
            r: r, ' -arch ppc '.
            myAssemblerSystem generators instructionTemplates gen64s: [r: r, ' -force_cpusubtype_ALL '].
            r, resend.asmArgs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         asmFilePrefix = ( |
            | 
            '.globl _', startSym, ', _', endSym, '\n',
            '_', startSym, ':\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         asmFileSuffix = ( |
            | '_', endSym, ': \n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         instructionTester = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> 'instructionTester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework tester parent instructionTester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc tester parent instructionTester.

CopyDowns:
globals assemblerSystems framework tester parent instructionTester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc tester parent instructionTester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         timeAssembler = ( |
             a.
             n = 10000.
             r.
             t1.
             t2.
            | 
            a: assembler copy.
            t1: [n do: [a addTo: a r3 From: a r4 With: a r5]] time.
            t2: [n do: [self]] time.
            r: (t1 asFloat - t2 asFloat) / n asFloat / 1000.0 "ms".
            r printLine.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         to_eq = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         to_gt = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         to_gtu = 16.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         to_lt = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'toMasks' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         to_ltu = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         asmPPC = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmPPC.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmPPC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.32 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPC' -> () From: ( | {
         'ModuleInfo: Module: asmPPC InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmPPCReg1
asmPPCBitRange
asmPPCGens'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmPPCReg1' From: 'applications/asmKit/asmPPC'
 bootstrap read: 'asmPPCBitRange' From: 'applications/asmKit/asmPPC'
 bootstrap read: 'asmPPCGens' From: 'applications/asmKit/asmPPC'



 '-- Side effects'

 globals modules asmPPC postFileIn
