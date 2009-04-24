 '$Revision: 30.18 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> () From: ( | {
         'Comment: All SPARC trademarks are used under license and are trademarks or registered
trademarks of SPARC International, Inc. in the US and other countries.
Products bearing SPARC trademarks are based upon an architecture developed
by Sun Microsystems, Inc.\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         sparc = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         assembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework assembler copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc assembler.

CopyDowns:
globals assemblerSystems framework assembler. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc assembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToLabel: lbl = ( |
            | 
            ba_aTo: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         commentSentinel = ( |
            | 
            fdivsFrom: f0 With: f0 ToFloat: f0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fields = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'fields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc fields.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fields* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'fields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: extracting bits\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         hi: i = ( |
            | (i && -16r400) >> 10).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         instructionAssemblyMethods = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'instructionAssemblyMethods' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc instructionAssemblyMethods.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         instructionAssemblyMethods* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'instructionAssemblyMethods' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: extracting bits\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         lo: i = ( |
            | i && 16r3ff).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems sparc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         operands* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc operands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         operands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'assembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: multi-line instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         set: i To: reg = ( |
            | setuw: i To: reg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: multi-line instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         setsw: i To: reg = ( |
            | 
            (0 <= i) && [(lo: i) = 0] ifTrue: [^sethi: (hi: i) To: reg].
            (-4096 <= i) && [i <= 4095] ifTrue: [^orFrom: go WithI: i To: reg].
            (i < 0) &&  [(lo: i) = 0] ifTrue: [
              sethi: (hi: i) To: reg.
              ^sraFrom: reg With: g0 To: reg.
            ].
            0 <= i ifTrue: [
              sethi: (hi: i) To: reg.
              ^orFrom: reg WithI: (lo: i) To: reg.
            ].
            sethi: (hi: i) To: reg.
            orFrom: reg WithI: (lo: i) To: reg.
            sraFrom: reg With: g0 To: reg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: multi-line instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         setuw: i To: reg = ( |
            | 
            (lo: i) = 0           ifTrue: [^sethi: (hi: i) To: reg].
            (0 < i) && [i < 4095] ifTrue: [^orFrom: g0 WithI: i To: reg].

            sethi: (hi: i) To: reg.
            orFrom: reg WithI: (lo: i) To: reg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: multi-line instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         setx: i WithTempReg: tempReg To: rd = ( |
            | 
            sethi: (uhi: i) To: tempReg.
            orFrom:   tempReg WithI: (ulo: i) To: tempReg.
            sllxFrom: tempReg WithI: 32       To: tempReg.
            sethi:  (hi: i) To: rd.
            orFrom:   rd      With:  tempReg  To: rd.
            orFrom:   rd      WithI:  (lo: i) To: rd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: extracting bits\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         uhi: i = ( |
            | hi: i >> 32).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'assembler' -> 'parent' -> () From: ( | {
         'Category: extracting bits\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         ulo: i = ( |
            | lo: i >> 32).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         bigEndianMixin* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: caches\x7fModuleInfo: Module: asmSPARC InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllRegisters.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledInstruction = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembledInstruction' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework disassembledInstruction copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembledInstruction' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc disassembledInstruction.

CopyDowns:
globals assemblerSystems framework disassembledInstruction. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembledInstruction' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembledInstruction' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc disassembledInstruction parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         externalOpcode = ( |
             n.
            | 
            n: resend.externalOpcode.

            ('pr' isSuffixOf: n) ifTrue: [^ n].
            case if: ['rd' isPrefixOf: n] Then: [(n copyAtMost: 2), ' %', (n copyFrom: 2), ', ']
                 If: ['wr' isPrefixOf: n] Then: ['wr']
                                          Else: [n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembledInstruction' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembledInstruction' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework disassembler copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc disassembler.

CopyDowns:
globals assemblerSystems framework disassembler. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembler' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc disassembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems sparc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'disassembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'disassembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fccOperands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'fccOperands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc fccOperands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'fprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc fprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         generators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'generators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc generators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         gprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'gprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc gprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         iccOperands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'iccOperands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc iccOperands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         instructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'instructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc instructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         loggingAssembler = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'loggingAssembler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework loggingAssembler _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'loggingAssembler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc loggingAssembler.

CopyDowns:
globals assemblerSystems framework loggingAssembler. _Clone 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'loggingAssembler' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'loggingAssembler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc loggingAssembler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         myAssemblerSystem = ( |
            | assemblerSystems sparc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'loggingAssembler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'loggingAssembler' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         membarOperands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'membarOperands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc membarOperands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fccOperands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'fccOperands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fprs* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'fprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         gprs* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'gprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         iccOperands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'iccOperands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         membarOperands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'membarOperands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         privilegedRegisters = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'privilegedRegisters' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc privilegedRegisters.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         privilegedRegisters* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'privilegedRegisters' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         pseudoInstructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'pseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc pseudoInstructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         tester = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework tester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc tester.

CopyDowns:
globals assemblerSystems framework tester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc tester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: testing\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmArgs = ( |
            | 
            generatingV9Instructions
              ifTrue: '-Wa,-xarch=v8plusa'
               False: '-Wa,-xarch=v8').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmBaddiesCommand = ( |
            | 
            '/bin/csh -c "', resend.asmBaddiesCommand, '"').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmFilePrefix = ( |
            | 
            '.section ".text",#alloc,#execinstr\n',
            '.align 8\n',
            '.skip 16\n',
            '.global ', startSym, '\n',
            '.type ', startSym, ',2\n',
            startSym, ':\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmFileSuffix = ( |
            | 
            '.global ', endSym, '\n',
            '.type ', endSym, ',2\n',
            endSym, ':\n\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: external assembly source hacks\x7fComment: I insert a \'nop\' after each instruction
in order to comply with whatever delay slot rules there are.
I assume that a \'nop\' is always a valid delay slot occupant.

Also help patch \'wr*\' instructions (A.62).
Take the remainder behind \'wr\' in the instruction name
and append it as a state register argument 
to the external instruction.
Example:    wrccr ... -> \'wr ..., %ccr\'\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         externalSourceFor: instructionTester = ( |
             n.
            | 
            n: instructionTester instructionTemplate name.

            (resend.externalSourceFor: instructionTester), 
            (('wr' isPrefixOf: n) && ('wrpr' != n) ifTrue: [',    %', (n copyFrom: 2)] False: ''),
            '     ; nop ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: external assembly source hacks\x7fComment: I skip an extra \'nop\' instruction.\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         extractInstructionFor: i From: bin = ( |
             r.
            | 
            r: resend.extractInstructionFor: i From: bin.
            binaryPos: binaryPos + 4.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         extractLineNumberFrom: line Into: block = ( |
             prefix.
             tokens.
            | 
            prefix: ' "', asmBadTestFileName, '", line '.
            tokens: (line asTokensSeparatedByCharactersIn: ':').
            tokens isEmpty not ifTrue: [tokens removeFirst].
            tokens isEmpty not && [prefix isPrefixOf: tokens first] ifTrue: [
                block value: ((tokens first copyFrom: prefix size) asIntegerIfFail: 0).
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         instructionTester = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework tester parent instructionTester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc tester parent instructionTester.

CopyDowns:
globals assemblerSystems framework tester parent instructionTester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc tester parent instructionTester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems sparc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'tester' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         sparcMixins* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'sparcMixins' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmSPARC = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmSPARC.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmSPARC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.18 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARC' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmSPARCBitRange
asmSPARCGens
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmSPARCBitRange' From: 'applications/asmKit/asmSPARC'
 bootstrap read: 'asmSPARCGens' From: 'applications/asmKit/asmSPARC'



 '-- Side effects'

 globals modules asmSPARC postFileIn
