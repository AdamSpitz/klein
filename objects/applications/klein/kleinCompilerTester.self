 '$Revision: 30.60 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compilerTestPrograms = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | method to compile).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: architectures\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         architectures = [ | x =  ( bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent') \/-> 'architectures') -> () From: ( |
                     {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms abstract parent architectures.
'.
                    | ) ) _Clone: 1 Filler: 0| 
             x _At: 0  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent') \/-> 'architectures') -> () _At: 0 Put: (
     'ppc')

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent') \/-> 'architectures') -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'vector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: architectures\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         architecturesDo: blk = ( |
            | architectures do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         areResults: nm OKFor: arch = ( |
             bcs.
             c.
             eds.
             firstDiff <- -1.
             is.
             oldCode.
             oldNM.
             qm.
             r.
             sourceIs.
             sourceWas.
             src.
             was.
            | 

            oldNM: oldCodeFor: arch.

            sourceIs:  disassemble:    nm For: arch.
            sourceWas: disassemble: oldNM For: arch.

            ["someday use comparator"
              c: comparator compare: sourceWas With: sourceIs.
            ].
            firstDiff:
             (nm size min: oldNM size) 
               findFirst: [|:i| (nm at: i) != (oldNM at: i)]
               IfPresent: [|:i| i]
                IfAbsent: [nm size min: oldNM size].

            src:    uglyTextEditorMorph copyString: '' Style: ( | color = paint | ).
            bcs:    uglyTextEditorMorph copyString: '' Style: ( | color = paint | ).
            was:    uglyTextEditorMorph copyString: '' Style: ( | color = paint | ).
            is:     uglyTextEditorMorph copyString: '' Style: ( | color = paint | ).

            eds: (src & bcs & was & is ) asVector.
            eds with: ('Source' & 'Bytecodes' & 'Was' & 'Is' ) asVector Do:
              [|:e. :t| e appendString: t, ':' ].
            eds with: (
              mySlotToCompile basicPrintStringAssignable &
              (mySlotToCompile isMethod ifTrue: [myMethod disassemble] False: '') &
              sourceWas &
              sourceIs 
            ) asVector Do: [|:e. :b| 
              e appendString: '\n', b "FontSpec: fsb FontColor: blk"].

            eds do: [|:e| e text selectionStart: 0@0 End: 0@0].

            eds: eds copyMappedBy: [|:e| 
              (frameMorph copy addMorph: e) frameStyle: frameMorph insetBezelStyle
            ].

            r: rowMorph copy topJustify.
            r addAllMorphs: eds.

            qm: userQueryMorph
              copyQuestion:
              'Test: ', asMirror name, ' failed for: ', arch, '. ',
              'First difference occurs at: ', firstDiff shortIfPossibleHexPrintString, '\n',
              'Is this correct?'.

            qm addButtonToRow:
              (( ui2Button copyColor: qm color Target: self)
                 label: 'Show NIC code')
                 scriptBlock: [target showNICCode].
            qm buttonInRowLabel: 'Incorrect' Result: false.
            qm buttonInRowLabel: 'Correct'   Result: true.
            qm morphsDo: [|:m| m morphTypeName = 'columnMorph' ifTrue: [
              m leftJustify.
              m addMorph: r. r: nil.
            ]].
            qm popUpWhereEventHappened: process this birthEvent.
            qm drawAttention awaitResponse.
            qm result ifFalse: [halt].
            qm result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         blockMethodSlotsToCompile: aSlot IfPresent: pb IfAbsent: ab = ( |
            | 
            aSlot contents blockLiteralsDo: [|:blockLit. m|
              m: reflect: blockLit.
              m method findFirst: [|:s| s key = 'compile_me']
                       IfPresent: [ ^ pb value: (aSlot & m valueSlot) asList ]
                        IfAbsent: [ blockMethodSlotsToCompile:  m valueSlot
                                                    IfPresent:  [|:slots| ^ pb value: slots addFirst: aSlot]
                                                     IfAbsent:  [] ].
            ].
            ab value: 'no block method with compile_me slot').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         blockMethodSlotsToCompileIfAbsent: ab = ( |
             outerMethodSlot.
            | 

            outerMethodSlot: asMirror 
              findFirst: [|:s| 'compile_block_method' isPrefixOf: s name]
              IfPresent: [|:s| s]
              IfAbsent:  [^ ab value: 'no slot to compile'].

            blockMethodSlotsToCompile: outerMethodSlot IfPresent: [|:slots| slots] IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: test results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         codeFor: arch Was: s = ( |
            | 
            [ppcCode: s]. "browsing"
            (arch, 'Code:') sendTo: self With: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         compile: aSlot For: arch LexicalParentScopes: lpss = ( |
             c.
             context.
            | 
            c: compilerPrototype.
            context: c prototypes compilationContext
                                    copyForSlot: aSlot
                                           Self: myMap "won't work yet"
                            LexicalParentScopes: lpss.
            c: c copyForContext: context
                   Architecture: arch
                         Oracle: c oracleThatCannotDoEagerRelocation
                          Debug: true
                       Optimize: c prototypes optimizationPolicies compileQuickly.
            c compileForcingNonLeafIfNecessary).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling and saving\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         compileAndSave = ( |
            | 
            _NakedMethods: true.
            regenerateTestIfAutomaticallyGenerated.
            architecturesDo: [|:a| compileAndSaveFor: a]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling and saving\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         compileAndSaveFor: arch = ( |
             aCompiler.
             ok.
             qm.
            | 
            codeFor: arch Was: compileMySlotFor: arch. 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         compileMySlot = ( |
            | 
            compileSlots: mySlotsToCompile For: theVM architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         compileMySlotFor: arch = ( |
            | 
            (newVMFor: arch) setTheVMAndDo: [compileMySlot]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         compileSlots: slots For: arch = ( |
             lastCompiler.
             outerNMethods.
            | 
            outerNMethods: list copyRemoveAll.

            slots do: [|:slot. compiler|
              compiler: 
                              compile: slot 
                                  For: arch 
               LexicalParentAllocator: lastCompiler ifNotNil: [|:c| c topSourceLevelAllocator]
                  LexicalParentScopes: outerNMethods.

              outerNMethods addLast: compiler buildNMethod.
              lastCompiler: compiler.
            ].
            outerNMethods last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         compilerPrototype = ( |
            | 
            vmPrototype compilerPrototype).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         disassemble: machineCode For: arch = ( |
            | 
            [ppc. sparc]. "browser"
            (arch sendTo: assemblerSystems)
              disassembler copy disassembleAllExternalSourceWithAddresses: machineCode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         myMethod = ( |
             slot.
            | 
            slot: mySlotToCompile.
            slot isMethod ifFalse: [error: 'not method'].
            slot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         mySlotToCompile = ( |
            | 
            mySlotsToCompile last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         mySlotsToCompile = ( |
            | 
            mySlotsToCompileIfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         mySlotsToCompileIfAbsent: blk = ( |
            | 
            asMirror findFirst: [|:s| 'compile_me' isPrefixOf: s key]
                     IfPresent: [|:s| vector copyAddFirst: s ]
                      IfAbsent: [ blockMethodSlotsToCompileIfAbsent: blk]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         newVMFor: arch = ( |
             vm.
            | 
            vm: vmPrototype copyForArchitecture: arch.
            vm lens: vm vmKit memoryLens.
            vm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: test results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         oldCodeFor: arch = ( |
            | 
             [ppcCode. sparcCode]. "browsing"
            (arch, 'Code') sendTo: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         regenerateTestIfAutomaticallyGenerated = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: resetting\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         reset = ( |
            | 
            architecturesDo: [|:a|
             resetForArchitecture: a
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: resetting\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         resetForArchitecture: arch = ( |
            | 
            codeFor: arch Was: '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling and saving\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         saveToFile: fileName ForArch: arch = ( |
             file.
             outString.
            | 
            outString: (reflect: self) name.
            outString: (outString, ' ', arch, 'Code: ').
            (sourceShouldBeFor: arch) size printLine.
            outString: (outString, '\'', (sourceShouldBeFor: arch) canonicalize escaped, '\'.\n').
            outString: outString canonicalize asByteVector.
            file: os_file copy openForAppending: fileName.
            file lseekOffset: 0 Whence: os_file seek_end.
            file write: outString.
            file close).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: showing NIC code\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         showNICCode = ( |
             cs.
             f = bootstrap stub -> 'globals' -> 'false' -> ().
             ls.
             n.
             t = bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            "open coded to avoid extra noise"
            cs: _Compilers.
            ls: _RecompileLimits.
            n: mySlotToCompile name canonicalize.
            "_Compilers: (vector copyAddFirst: 'nic') Limits: vector."
            "_PrintCompilation: t."
            "_PrintCompiledCode: t."

            _BugHuntNames: vector copyAddFirst: n.

            _Flush.

            n sendTo: self WithArguments: vector copySize: n asSelector numberOfArguments.

            _BugHuntNames: vector.

            _PrintCompiledCode: f.
            _PrintCompilation: f.
            "_Compilers: cs Limits: ls."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         test = ( |
            | 
            _NakedMethods: true.
            regenerateTestIfAutomaticallyGenerated.
            architecturesDo: [|:a| testFor: a]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         testFor: arch = ( |
             nm.
             ok.
             oldNM.
             qm.
            | 
            nm: compileMySlotFor: arch. 
            oldNM: oldCodeFor: arch.
            nm = oldNM ifTrue: [^ true].

            ok: areResults: nm OKFor: arch.
            ok ifTrue: [codeFor: arch Was: nm]
                False: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         vmPrototype = ( |
            | 
            klein virtualMachines selfVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         ppcCode <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractBlockMethod = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms abstractBlockMethod.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [ | compile_me = 0 | 17 ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms abstractBlockMethod parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: primitives\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         add = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'add' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'add' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms add.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'add' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a = ( |
            | 
            _IntAdd: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'add' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'add' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms add parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'add' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: primitives\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         addFailures = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'addFailures' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'addFailures' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms addFailures.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'addFailures' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             a <- 1.
             b <- 2.
             c.
             failBlk.
            | 
            c: _IntAdd: 1 + 2.
            c: _IntAdd: 1 + 'a'.
            c: _IntAdd: 'b' + 1.
            c: _IntAdd: maxSmallInt + 1.
            c: _IntAdd: 1 + 'a' IfFail: failBlk.
            c: _IntAdd: 'b' + 1 IfFail: failBlk.
            c: _IntAdd: maxSmallInt + 1 IfFail: failBlk.
            c: a _IntAdd: b. "check for type and overflow errors"
            a _IntAdd: b IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'addFailures' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'addFailures' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms addFailures parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'addFailures' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         alloc2 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms alloc2.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         b = ( |
            | 12).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            obj r: obj a _IntAdd:  obj b "breaks and puts this in r30").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         eleven = 11.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         obj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'obj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms alloc2 obj.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (1)'
        
         a <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         b = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'obj' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms alloc2 obj parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (nil)'
        
         r.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms alloc2 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (10)'
        
         ten <- 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'alloc2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (nil)'
        
         x.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         allocR4 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'allocR4' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'allocR4' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms allocR4.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'allocR4' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: i = ( |
            | 
            compile_me: i _IntAdd: -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'allocR4' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'allocR4' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms allocR4 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'allocR4' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         argsAndLocalsLeaf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsLeaf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsLeaf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms argsAndLocalsLeaf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a B: b C: c = ( |
             d <- 1.
             e <- 2.
             f <- 3.
             g <- 4.
             z1 = 1.
             z2 = 2.
             z3 = 3.
            | 
            "Is leaf because # args + # locals <= maxArgumentRegisters = 7"

            z3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsLeaf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms argsAndLocalsLeaf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsLeaf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         argsAndLocalsNonLeaf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsNonLeaf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsNonLeaf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms argsAndLocalsNonLeaf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsNonLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a B: b C: c = ( |
             d <- 1.
             e <- 2.
             f <- 3.
             g <- 4.
             h <- 5.
             z1 = 1.
             z2 = 2.
             z3 = 3.
            | 
            "Is leaf because # args + # locals > maxArgumentRegisters = 7"

            z3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsNonLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsNonLeaf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms argsAndLocalsNonLeaf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'argsAndLocalsNonLeaf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         autoGenerated = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms autoGenerated.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms autoGenerated parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         buildMethodComment = ( |
            | 'auto-generated').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         buildMethodContents = ( |
            | 
            buildMethodSource parseObjectBodyIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         buildMethodName = ( |
            | 
            'compile_me').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         buildMethodSlot = ( |
             newSlot.
            | 
            newSlot: slots method.
            newSlot: newSlot copyHolderForName:     buildMethodName.
            newSlot: newSlot copyHolderForContents: buildMethodContents.
            newSlot: newSlot copyHolderForComment:  buildMethodComment.
            newSlot: newSlot copyHolderForModule:   'init'.
            newSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         buildMethodSource = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         discardTest = ( |
            | 
            asMirror removeSlot: (mySlotsToCompileIfAbsent: [^ self]) last name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         generateTest = ( |
            | 
            asMirror addSlot: buildMethodSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         regenerateTestIfAutomaticallyGenerated = ( |
            | 
            discardTest.
            generateTest).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         reset = ( |
            | resend.reset.
            discardTest).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksAssignBlockResultUsingLexicalVariables = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksAssignBlockResultUsingLexicalVariables' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksAssignBlockResultUsingLexicalVariables' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksAssignBlockResultUsingLexicalVariables.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksAssignBlockResultUsingLexicalVariables' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method: a = ( |
             b.
             c = 17.
             compile_me = 0.
            | 
            snort: [|compile_me=0| (a * b) - c]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksAssignBlockResultUsingLexicalVariables' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksAssignBlockResultUsingLexicalVariables' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksAssignBlockResultUsingLexicalVariables parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksAssignBlockResultUsingLexicalVariables' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksBasic = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBasic' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBasic' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksBasic.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBasic' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [|compile_me=0| 17] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBasic' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBasic' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksBasic parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBasic' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksBlockLocalVariable = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockLocalVariable' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockLocalVariable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksBlockLocalVariable.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockLocalVariable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [|compile_me=0. foo=17| foo] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockLocalVariable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockLocalVariable' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksBlockLocalVariable parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockLocalVariable' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksBlockNotInUse = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockNotInUse' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockNotInUse' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksBlockNotInUse.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockNotInUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [ | compile_me = 0 | 17 ] value.
            20).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockNotInUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockNotInUse' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksBlockNotInUse parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksBlockNotInUse' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksDataSlotAccess = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksDataSlotAccess.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (17)'
        
         a <- 17.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            "ensure that load of a is from self, not block"
            [ | compile_me = 0 | a ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksDataSlotAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksDataSlotAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNLR = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNLR' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNLR' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNLR.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNLR' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [|compile_me=0| ^17] value.
            18).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNLR' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNLR' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNLR parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNLR' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNested_BlockNotInUse = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_BlockNotInUse.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [ | compile_me = 0 | 17 ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            [[17] value] value.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_BlockNotInUse parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_BlockNotInUse' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNested_ComplexLexicalVariableUse = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ComplexLexicalVariableUse' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ComplexLexicalVariableUse' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_ComplexLexicalVariableUse.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ComplexLexicalVariableUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
             a = 1.
            | 
            [|b=2|
              [|compile_me=0. c=3| a+b+c] value
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ComplexLexicalVariableUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ComplexLexicalVariableUse' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_ComplexLexicalVariableUse parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ComplexLexicalVariableUse' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNested_NLR = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_NLR' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_NLR' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_NLR.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_NLR' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [[|compile_me=0| ^17] value] value.
            18).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_NLR' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_NLR' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_NLR parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_NLR' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNested_ReturnInnerSelf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnInnerSelf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnInnerSelf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_ReturnInnerSelf.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnInnerSelf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [[|compile_me=0| c] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnInnerSelf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnInnerSelf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_ReturnInnerSelf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnInnerSelf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNested_ReturnOuterSelf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnOuterSelf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnOuterSelf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_ReturnOuterSelf.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnOuterSelf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [|compile_me=0| [17] value. self] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnOuterSelf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnOuterSelf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_ReturnOuterSelf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_ReturnOuterSelf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksNested_SimpleLexicalVariableUse = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_SimpleLexicalVariableUse' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_SimpleLexicalVariableUse' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_SimpleLexicalVariableUse.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_SimpleLexicalVariableUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
             foo = 17.
            | 
            [[|compile_me=0| foo] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_SimpleLexicalVariableUse' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_SimpleLexicalVariableUse' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksNested_SimpleLexicalVariableUse parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksNested_SimpleLexicalVariableUse' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksPassBlockToSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksPassBlockToSlot.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            test: [|compile_me=0| 17]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksPassBlockToSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         test: blk = ( |
            | 
            blk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksPassBlockToSlot_UseLexicalVariable = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksPassBlockToSlot_UseLexicalVariable.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
             foo <- 17.
            | 
            a: 1 B: 2 C: 3 D: 4.
            test: [|compile_me=0| foo]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksPassBlockToSlot_UseLexicalVariable parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksPassBlockToSlot_UseLexicalVariable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         test: blk = ( |
            | 
            blk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksSimpleNested_CompileInner = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileInner' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileInner' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksSimpleNested_CompileInner.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileInner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [[|compile_me=0| 17] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileInner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileInner' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksSimpleNested_CompileInner parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileInner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: blocks\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksSimpleNested_CompileOuter = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileOuter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileOuter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksSimpleNested_CompileOuter.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [|compile_me=0| [17] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileOuter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms blocksSimpleNested_CompileOuter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'blocksSimpleNested_CompileOuter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compileAll = ( |
            | 
            testsDo: [|:t| t compileAndSave].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compileWorkingTests = ( |
            | 
            testsDo: [|:t|
              t = addFailures ifTrue: [^ self].
              t compileAndSave
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: primitives\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         compilingAllPrimitives = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'compilingAllPrimitives' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms autoGenerated copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'compilingAllPrimitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms compilingAllPrimitives.

CopyDowns:
globals klein compilerTestPrograms autoGenerated. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'compilingAllPrimitives' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'compilingAllPrimitives' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms compilingAllPrimitives parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'compilingAllPrimitives' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         buildMethodSource = ( |
             body <- ''.
             browsingTagMiddle = '*_*'.
             browsingTagPrefix = '['.
             browsingTagSuffix = ']. \"browsing\"'.
             slots.
             wholeTag.
            | 
            [generatePrimitive_IntAddIfFail_: fb]. "browsing"
            slots: (compilerPrototype copy architecture: 'ppc') 
                      protoCodeGenForMyPlatform primitiveGenerationMethodSlots,
                   klein primitives fakePrimitiveMethodSlots.

            wholeTag: browsingTagPrefix, browsingTagMiddle, browsingTagSuffix.

            "assemble a method containing all browsing tags"
            slots do: [|:s. tagsFound <- 0. | 
              s contents source asTextLines do: [|:l. line|
                line: l shrinkwrapped.
                (line matchesPattern: wholeTag) ifTrue: [|tag|
                  tag:  line copyFrom: browsingTagPrefix size UpTo: line size - browsingTagSuffix size.
                  body: body, tag, '.\n'.
                  tagsFound: tagsFound succ.
                ]
              ].

              (tagsFound < 2) ifTrue: [
                "We expect two browsing tags in each primitive code
                 generator method for testing compilation of the primitive.
                 One tag should specify the non-IfFail version, and another
                 should specify the IfFail version.
                 -- jb 7/03"
                error: 'Expected at least 2 browsing tags in ', s name.
              ]
            ].
            body).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'compilingAllPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'autoGenerated' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         constantMergeTest = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'constantMergeTest' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'constantMergeTest' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms constantMergeTest.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'constantMergeTest' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            0 __BranchIfFalse: 1 To: 'end'.
            2 __DefineLabel: 'end').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'constantMergeTest' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'constantMergeTest' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms constantMergeTest parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'constantMergeTest' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionStackLeaf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackLeaf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackLeaf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms expressionStackLeaf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a B: b C: c D: d = ( |
             e <- 0.
             f <- 1.
            | 
            "No sends and: #arg + #locals = 6 = maxArgumentRegisters - 1.
             We need space for a stack location to place the
             result of each expression (notice 'pop' nodes),
             so we use the last volatile register, thus we CAN
             apply the leaf method optimization. -- jb 8/03"
            e.
            f.
            e: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackLeaf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms expressionStackLeaf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackLeaf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionStackNonLeaf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackNonLeaf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackNonLeaf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms expressionStackNonLeaf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackNonLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a B: b C: c D: d = ( |
             e <- 0.
             f <- 1.
             g.
            | 
            "No sends and: #arg + #locals = 7 = maxArgumentRegisters
             We need space for a stack location to place the
             result of each expression (notice 'pop' nodes)
             but no volatile registers remain, so we CANNOT
             apply the leaf method optimization. -- jb 8/03"
            e.
            f.
            e: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackNonLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackNonLeaf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms expressionStackNonLeaf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'expressionStackNonLeaf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: sends\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupStub = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lookupStub' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lookupStub' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms lookupStub.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lookupStub' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             del.
             i <- 0.
             key.
             lookupKeyPrototype = bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> ().
             lt.
             map.
             maskedLT.
             n.
             nilOop = bootstrap stub -> 'globals' -> 'nil' -> ().
             nm.
             nmc.
             nmethodPrototype = bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> ().
             sd.
             sel.
            | 

            "Keep the sendMessage_stub in sync with klein compilerTestPrograms lookupStub."

            _NoMapTest.
            _VariableArguments.
            _NoGCAllowed.
            _NoSendsAllowed.

            sd: _CallingSendDesc.
            sel: sd _SendDescSelector.
            lt:  sd _SendDescLookupType.
            del: sd _SendDescDelegatee.
            map: _Map.
            nmc: map _NMethodCache.
            n: nmc _Size.

            "Maybe use AltiVec for this loop someday. -- Dave and Adam and Alex, 3/04"

            "We're doing a lot of boolean-loading. -- Adam & Alex, 4/04"

            "Loop through the nmethod cache until we find an nmethod
             with a matching lookupKey. We need to compare the selector,
             lookup type, and delegatee. (Note that the 'delegatee'
             for an undirected resend is really the object that holds
             the method that is doing the resend. And for a normal
             lookup, it's always nil. We should really name it
             something better.)"

            maskedLT: lt _MaskedLookupType.

            __DefineLabel: 'loopHead'.
            __BranchIfTrue: (i _IntEQ: n) To: 'miss'.
            nm:  nmc _At: i.
            key:  _In: nm WhichIsOfType: nmethodPrototype Get: 'lookupKey'.
            __BranchIfFalse: ((_In: key WhichIsOfType: lookupKeyPrototype Get: 'selector'  )                   _Eq: sel     ) To: 'notFoundYet'.
            __BranchIfFalse: ((_In: key WhichIsOfType: lookupKeyPrototype Get: 'lookupType') _MaskedLookupType _Eq: maskedLT) To: 'notFoundYet'.
            __BranchIfFalse: ((_In: key WhichIsOfType: lookupKeyPrototype Get: 'delegatee' )                   _Eq: del     ) To: 'notFoundYet'.
            __BranchTo: 'found'.

            __DefineLabel: 'notFoundYet'.
            i: i _IntAdd: 1.
            __BranchTo: 'loopHead'.


            __DefineLabel: 'found'.

            sd _BackpatchSendDescTo: nm _NMethodEntryPoint Map: map.
            "_Breakpoint: 'hit: about to retry sendDesc'."
            sd _RetrySendDesc.
            _Breakpoint: 'should never reach here'.

            __DefineLabel: 'miss'.
            _Breakpoint: 'miss, about to return selector'.
            sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lookupStub' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lookupStub' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms lookupStub parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lookupStub' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         lotsaArgsLeaf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsLeaf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsLeaf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms lotsaArgsLeaf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a B: b C: c D: d E: e F: f G: g H: h I: i J: j L: l M: m = ( |
            | 
            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsLeaf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms lotsaArgsLeaf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsLeaf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         lotsaArgsNonLeaf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsNonLeaf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsNonLeaf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms lotsaArgsNonLeaf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsNonLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a B: b C: c D: d E: e F: f G: g H: h I: i J: j L: l M: m = ( |
            | m foobar: a B: b C: c D: d E: e
            F: f G: g H: h I: i J: j L: l M: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsNonLeaf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsNonLeaf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms lotsaArgsNonLeaf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'lotsaArgsNonLeaf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         manyLocals = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'manyLocals' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'manyLocals' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms manyLocals.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'manyLocals' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             a <- 1.
             b <- 2.
             c <- 3.
             d <- 4.
             e <- 5.
             f <- 6.
             g <- 7.
             h <- 8.
            | 
            g _IntAdd: h.
            g: h.
            a: h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'manyLocals' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'manyLocals' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms manyLocals parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'manyLocals' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: allocator\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         multipop = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'multipop' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'multipop' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms multipop.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'multipop' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            bar: foo __BranchIfFalse: 0 To: 'end'.
            1 __DefineLabel: 'end').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'multipop' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'multipop' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms multipop parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'multipop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: non-local returns\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         nlr = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlr' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlr' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms nlr.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlr' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            [^ 42] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlr' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlr' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms nlr parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlr' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: non-local returns\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         nlrBranchTest = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlrBranchTest' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_block_method' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstractBlockMethod copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlrBranchTest' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms nlrBranchTest.

CopyDowns:
globals klein compilerTestPrograms abstractBlockMethod. copy 
SlotsToOmit: compile_block_method parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlrBranchTest' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_block_method = ( |
            | 
            [|compile_me = 0|
             3.
             1 __BranchTo: 'end'.
             ^ 2 __DefineLabel: 'end'
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlrBranchTest' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlrBranchTest' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms nlrBranchTest parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'nlrBranchTest' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstractBlockMethod' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: data slots\x7fCategory: accessing on other object\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         otherDataAssignment = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms otherDataAssignment.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (2)'
        
         compile_me <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms otherDataAssignment parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         mySlotToCompileIfAbsent: blk = ( |
            | (reflect: self) at: 'compile_me:' IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: data slots\x7fCategory: accessing on other object\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         otherDataConst = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataConst' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataConst' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms otherDataConst.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataConst' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataConst' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataConst' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms otherDataConst parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataConst' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: data slots\x7fCategory: accessing on other object\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         otherDataVar = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataVar' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataVar' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms otherDataVar.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataVar' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (23)'
        
         compile_me <- 23.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataVar' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataVar' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms otherDataVar parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'otherDataVar' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: \'real world\' tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         recursiveFactorial = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'recursiveFactorial' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'recursiveFactorial' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms recursiveFactorial.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'recursiveFactorial' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: i = ( |
             j <- 1.
            | 
            0 __BranchIfTrue: (i _Eq: 1) To: 'end'.
            j: (compile_me: i _IntAdd: -1) _IntMul: i.
            0 __DefineLabel: 'end'.
            j).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'recursiveFactorial' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'recursiveFactorial' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms recursiveFactorial parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'recursiveFactorial' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         resetAll = ( |
            | 
            testsDo: [|:t| t reset]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: returning results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         returnArg1 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnArg1.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: z1 = ( |
            | z1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg1' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnArg1 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: returning results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         returnArg2 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnArg2.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: a1 And: a2 = ( |
            | 
            a2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg2' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnArg2 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnArg2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: returning results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         returnConstantLocal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnConstantLocal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnConstantLocal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnConstantLocal.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnConstantLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             a.
             b = 17.
            | 
            b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnConstantLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnConstantLocal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnConstantLocal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnConstantLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: returning results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         returnLocal2 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnLocal2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnLocal2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnLocal2.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnLocal2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             a.
             b <- 3.
             c1 = 2.
             c3 = 4.
            | 
            b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnLocal2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnLocal2' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnLocal2 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnLocal2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: returning results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         returnSelf = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnSelf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnSelf' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnSelf.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnSelf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnSelf' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnSelf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnSelf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnSelf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: returning results\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         returnThree = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnThree' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnThree' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnThree.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnThree' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnThree' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnThree' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms returnThree parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'returnThree' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         runBenchmark = ( |
             t.
            | 
            t: [compileWorkingTests] time.
            t printLine.
            t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: data slots\x7fCategory: accessing on self\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         selfData = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms selfData.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            var: const _IntAdd: var.
            pvar: pvar _IntAdd: pconst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         const = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms selfData parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (nil)'
        
         pconst.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (nil)'
        
         pvar.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'selfData' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (1)'
        
         var <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: sends\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         sendAndReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendAndReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendAndReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms sendAndReturn.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendAndReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me: i = ( |
            | 
            0 __BranchIfFalse: (i _Eq: 0) To: 'jsut'.
            compile_me: 1.
            0 __DefineLabel: 'jsut'.
            i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendAndReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendAndReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms sendAndReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendAndReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: sends\x7fCategory: obsolete\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         sendMessage = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendMessage' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendMessage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms sendMessage.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendMessage' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            _SendMessage).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendMessage' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendMessage' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms sendMessage parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'sendMessage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: \'real world\' tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         simpleAdds = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleAdds' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleAdds' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms simpleAdds.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleAdds' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             a.
            | 
            "ensure no register is used for the sum"
            3 _IntAdd: 4.
            3 _IntAdd: a.
            a _IntAdd: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleAdds' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleAdds' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms simpleAdds parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleAdds' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: \'real world\' tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         simpleFactorial = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleFactorial' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleFactorial' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms simpleFactorial.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleFactorial' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
             i.
             r.
            | 
            i: 5.
            r: 1.
            0 __DefineLabel: 'repeat'.
            0 __BranchIfTrue: (i _Eq: 1) To: 'end'.
            r: r _IntMul: i.
            i: i _IntAdd: -1.
            0 __BranchTo: 'repeat'.
            0 __DefineLabel: 'end'.
            r _Breakpoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleFactorial' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleFactorial' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms simpleFactorial parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleFactorial' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: \'real world\' tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         simpleSend = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleSend' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleSend' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms simpleSend.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleSend' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            a: 1 B: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleSend' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleSend' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms simpleSend parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'simpleSend' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: tests\x7fCategory: method slots\x7fCategory: primitives\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         systemCall = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'systemCall' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'compile_me' From:
             bootstrap remove: 'parent' From:
             globals klein compilerTestPrograms abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'systemCall' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms systemCall.

CopyDowns:
globals klein compilerTestPrograms abstract. copy 
SlotsToOmit: compile_me parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'systemCall' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         compile_me = ( |
            | 
            _NoGCAllowed.
            (
              _SystemCall: 4 With: (1                 _IntForCIfFail: [^ -1]) 
                             With: ('abc'      _ByteVectorForCIfFail: [^ -1])
                             With: ('abc' _ByteSize   _IntForCIfFail: [^ -1])
            ) _IntFromCIfFail: [^ -1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'systemCall' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'systemCall' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTestPrograms systemCall parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'systemCall' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         testAll = ( |
            | 
            testsDo: [|:t| t test].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         testsDo: blk = ( |
            | 
            testsToRun do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTestPrograms' -> () From: ( | {
         'Category: running the tests\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         testsToRun = ( |
            | 
            (
            systemCall &

            "returning results"
            returnSelf &
            returnArg1 &
            returnArg2 &
            returnLocal2 &
            returnThree &
            returnConstantLocal &

            "data slots"
            selfData & 
            otherDataConst &
            otherDataVar &
            otherDataAssignment &

            "allocator"
            alloc2 & 
            allocR4 &
            lotsaArgsLeaf &
            lotsaArgsNonLeaf &
            argsAndLocalsLeaf &
            argsAndLocalsNonLeaf &
            expressionStackLeaf &
            expressionStackNonLeaf &
            manyLocals  & 
            constantMergeTest &
            multipop &

            "sends"
            sendAndReturn &
            lookupStub &

            "blocks"
            blocksBasic &
            blocksDataSlotAccess &
            blocksSimpleNested_CompileOuter &
            blocksSimpleNested_CompileInner &
            blocksBlockNotInUse &
            blocksNested_BlockNotInUse &
            blocksNLR &
            blocksNested_NLR &
            blocksBlockLocalVariable &
            blocksNested_SimpleLexicalVariableUse &
            blocksNested_ComplexLexicalVariableUse &
            blocksAssignBlockResultUsingLexicalVariables &
            blocksNested_ReturnInnerSelf &
            blocksNested_ReturnOuterSelf &
            blocksPassBlockToSlot &
            blocksPassBlockToSlot_UseLexicalVariable &

            "nlr"
            nlr &
            nlrBranchTest &

            "primitives"
            add &
            addFailures &
            compilingAllPrimitives &

            "real-world"
            simpleAdds &
            simpleSend & 
            simpleFactorial &
            recursiveFactorial
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         compilerTesterMorph = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals abstractSimpleApplicationMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTesterMorph.

CopyDowns:
globals abstractSimpleApplicationMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compilerTesterMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         addButtonsToMenu: menu = ( |
            | 
            addDuplicateButtonTo: menu.
            menu addButtonTarget: self
              AsynchronousScriptBlock: [ klein compilerTestPrograms testAll ] 
              Label: 'Test All'.
            menu addButtonTarget: self
              ScriptBlock: [ klein compilerTestPrograms compileAll ] 
              Label: 'Compile All'.
            menu addButtonTarget: self
              ScriptBlock: [ klein compilerTestPrograms resetAll ] 
              Label: 'Reset All'.
            menu addButtonTarget: self
              ScriptBlock: [ klein compilerTestPrograms runBenchmark ] 
              Label: 'Run Benchmark'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         addLabelIfNeeded = ( |
            | addLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: background menu\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         backgroundMenuButtonLabel = 'Klein compiler tester'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         defaultButtonHolder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: style\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         defaultColor = paint copyRed: 0.352884 Green: 0.863148  Blue: 0.814272.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'kleinCompilerTesterMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractSimpleApplicationMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         titleString = 'Klein compiler tester'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compilerTesterMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            klein compilerTesterMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         kleinCompilerTester = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinCompilerTester.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            [|:m|
              m initializePrototype.
              worldMorph addBackgroundMenuContributor: m.
            ] value: 
                klein compilerTesterMorph.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.60 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompilerTester' -> () From: ( | {
         'ModuleInfo: Module: kleinCompilerTester InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinCompilerTester postFileIn
