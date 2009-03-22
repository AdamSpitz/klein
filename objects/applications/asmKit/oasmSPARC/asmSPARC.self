 '$Revision: 30.5 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         sparc = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         asm = bootstrap define: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblers framework asm copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc asm.

CopyDowns:
globals assemblers framework asm. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc asm parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fields = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'fields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc fields.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fields* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'fields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         instructionAssemblyMethods = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'instructionAssemblyMethods' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc instructionAssemblyMethods.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         instructionAssemblyMethods* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'instructionAssemblyMethods' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         operands* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc operands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         operands* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'asm' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblers' -> 'framework' -> 'asm' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: public'
        
         disasm = bootstrap define: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'disasm' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblers framework disasm copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'disasm' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc disasm.

CopyDowns:
globals assemblers framework disasm. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'disasm' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'disasm' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc disasm parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'disasm' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblers' -> 'framework' -> 'disasm' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fccOperands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'fccOperands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc fccOperands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'fprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc fprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         generators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'generators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc generators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         gprs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'gprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc gprs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         iccOperands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'iccOperands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc iccOperands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         instructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'instructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc instructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         membarOperands = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'membarOperands' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc membarOperands.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fccOperands* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'fccOperands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         fprs* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'fprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         gprs* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'gprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         iccOperands* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'iccOperands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         membarOperands* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'membarOperands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         privilegedRegisters = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'privilegedRegisters' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc privilegedRegisters.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'operands' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         privilegedRegisters* = bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'privilegedRegisters' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblers' -> 'framework' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: name spaces of the SPARC architecture\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         pseudoInstructionTemplates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'pseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc pseudoInstructionTemplates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         tester = bootstrap define: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblers framework tester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc tester.

CopyDowns:
globals assemblers framework tester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc tester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fCategory: testing\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmArgs = '-Wa,-xarch=v8plus'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmBaddiesCommand = ( |
            | 
            '/bin/csh -c "', resend.asmBaddiesCommand, '"').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: generating external files\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         asmFileSuffix = ( |
            | 
            '.global ', endSym, '\n',
            '.type ', endSym, ',2\n',
            endSym, ':\n\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
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
            n: instructionTester myInstructionTemplate name.

            (resend.externalSourceFor: instructionTester), 
            (('wr' isPrefixOf: n) && ('wrpr' != n) ifTrue: [',    %', (n copyFrom: 2)] False: ''),
            '     ; nop ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: external assembly source hacks\x7fComment: I skip an extra \'nop\' instruction.\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         extractInstructionFor: i From: bin = ( |
             r.
            | 
            r: resend.extractInstructionFor: i From: bin.
            binaryPos: binaryPos + 4.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: testing bad instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         extractLineNumberFrom: line Into: block = ( |
             prefix.
             tokens.
            | 
            prefix: ' "', asmBadTestFileName, '", line '.
            tokens: (line asTokensSeparators: ':').
            tokens isEmpty not ifTrue: [tokens removeFirst].
            tokens isEmpty not && [prefix isPrefixOf: tokens first] ifTrue: [
                block value: ((tokens first copyFrom: prefix size) asIntegerIfFail: 0).
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test instructions\x7fModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         instructionTester = bootstrap define: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblers framework tester parent instructionTester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc tester parent instructionTester.

CopyDowns:
globals assemblers framework tester parent instructionTester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblers sparc tester parent instructionTester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         externalOpcodeFor: name = ( |
             n.
            | 
            n: name.

            ('pr' isSuffixOf: n) ifTrue: [^ n].
            case if: ['rd' isPrefixOf: name] Then: [(name copyAtMost: 2), ' %', (name copyFrom: 2), ', ']
                 If: ['wr' isPrefixOf: name] Then: ['wr']
                                             Else: [n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblers' -> 'framework' -> 'tester' -> 'parent' -> 'instructionTester' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblers' -> 'sparc' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARC InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblers' -> 'framework' -> 'tester' -> 'parent' -> ().
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
        
         revision <- '$Revision: 30.5 $'.
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
