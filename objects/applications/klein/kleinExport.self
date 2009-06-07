 '$Revision: 30.64 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object table\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         desiredStartingSizeForObjectTable: t = ( |
            | 
            "This method used to double the size, to
             leave some extra room so that we don't have
             to grow it inside the image. Now we do the
             doubling inside" [expectedNumberOfObjects: n].
            "That way, we're less likely to need to grow
             the object table after we've mapped it.
             -- Adam, 5/06"

            t size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         estimatedSizeOfBlock: b AssumingOopSizeIs: oopSize = ( |
            | 
            (3 * oopSize "for header and value slot") + (estimatedSizeOfMethod: (reflect: b) valueSlot contents AssumingOopSizeIs: oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         estimatedSizeOfLiteral: lit AssumingOopSizeIs: oopSize = ( |
            | 
            (reflect: lit) isReflecteeBlock  ifTrue: [^ estimatedSizeOfBlock:  lit AssumingOopSizeIs: oopSize].
            (reflect: lit) isReflecteeString ifTrue: [^ estimatedSizeOfString: lit AssumingOopSizeIs: oopSize].
            oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         estimatedSizeOfMethod: m AssumingOopSizeIs: oopSize = ( |
             lits.
            | 
            lits: m literals.
            (lits size * oopSize) + (lits copyMappedBy: [|:lit| estimatedSizeOfLiteral: lit AssumingOopSizeIs: oopSize]) sum + m codes size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         estimatedSizeOfSlot: s AssumingOopSizeIs: oopSize = ( |
            | 
            s contents isReflecteeMethod ifTrue: [^ estimatedSizeOfMethod: s contents AssumingOopSizeIs: oopSize].
            oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         estimatedSizeOfString: s AssumingOopSizeIs: oopSize = ( |
            | 
            s size + (3 * oopSize "for the object header and size field")).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         ifSlot: s HasExpectedOffsetThen: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         ifSlot: s ShouldHaveSpecialInitialContentsThen: blk = ( |
            | 
            "May be overridden by child."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         initialContentsOfSlot: s = ( |
             chain.
             path.
            | 

            ifSlot: s ShouldHaveSpecialInitialContentsThen: [|:c| ^ c].

            s isFake                                                            ifTrue: [^ s contents reflectee].
            s isAssignment                                                      ifTrue: [^ s contents reflectee].

            (namesOfModulesWhichShouldIgnoreTransporterLink includes: s module) ifTrue: [^ s contents reflectee].

            path: s holder creatorPathIfPresent: [|:p| p extendedBy: s name]
                                       IfAbsent: [^ s contents reflectee].

            chain: transporter chains lobbyLink forPath: path.

            transporter fileOut follow: chain
                              IfExpand: [s contents reflectee]
                               IfRefer: [s contents reflectee]
                          IfExpression: [s initialContents expressionValue]
                                IfFail: [s copyDownInfoIfPresent: [|:cd| (cd copiedParentMirror at: s name) contents reflectee]
                                                        IfAbsent: [error: 'error or unimplemented case']]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCachedModuleNameLists = ( |
            | 
            modulesToMap     invalidateCachedModuleNameList.
            modulesToCompile invalidateCachedModuleNameList.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Child can override to include additional objects, such
as ones that function as namespaces.  -- jb 8/03\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         isOkayToCompileMethodsForReflecteeOf: rMir = ( |
            | 
                rMir isComplete
            || [rMir isReflecteeVector              "vectors are not complete, but we want to compile vector methods"
            || [rMir isReflecteeByteVector
            || [(wellKnownIncompleteObjectsWithSlotsToCompile includes: rMir)
            || [rMir isReflecteeEmptyBlock "when you type [], you don't get a real block" ]]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeCompiled: s = ( |
            | 
            [originalBlock_replaceThisSlotWithTheValueSlot.
             originalBlock_replaceThisSlotWithTheValueSlot: nil]. "browsing"
            s name = 'originalBlock_replaceThisSlotWithTheValueSlot' ifTrue: [^ false].

            (isSlotToBeMapped: s) && [modulesToCompile includesModuleNamed: s module]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeMapped: s = ( |
            | 
            "Slot must exist."
            s exists ifFalse: [^ false].

            "Export all slots in methods."
            s holder isReflecteeMethod ifTrue: [^ true].

            (shouldAlwaysMapSlotNoMatterWhatModuleItIsIn: s) ifTrue: [^ true].

            (shouldNeverMapSlotNoMatterWhatModuleItIsIn:  s) ifTrue: [^ false].

            "Export by module (includes slots in unspecified modules)."
            (modulesToMap includesModuleNamed: s module) ifFalse: [^ false].

            "Don't export slots in a category starting with 'unmapped'."
            "Note: this was a bit slow when it said matchesPattern: '*unmapped*',
             so we changed it to just be a prefix check. -- Adam, 12/04"
            ('unmapped' isPrefixOf: s category) ifTrue: [^ false].

            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         modulePartitioningProto = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy modulePartitioningProto.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy modulePartitioningProto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         allNamesOfIncludedModules = ( |
            | 
            cachedAllNamesOfIncludedModules ifNil: [
              cachedAllNamesOfIncludedModules: set copyRemoveAll.
              namesOfModulesWhoseSubmodulesShouldBeIncludedToo do: [|:moduleName|
                module: (moduleName sendTo: modules) AndIncludedSubmodulesDo: [|:m|
                  cachedAllNamesOfIncludedModules add: m name canonicalize.
                ].
              ].
              namesOfIncludedModules mapBy: [|:s| s canonicalize] Into: cachedAllNamesOfIncludedModules.
              cachedAllNamesOfIncludedModules
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         includesModuleNamed: n = ( |
            | 
            allNamesOfIncludedModules includes: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCachedModuleNameList = ( |
            | 
            cachedAllNamesOfIncludedModules: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         module: aModule AndIncludedSubmodulesDo: blk = ( |
            | 
            (namesOfExcludedModules includes: aModule name) ifFalse: [
              blk value: aModule.
              aModule subparts do: [|:m| module: m AndIncludedSubmodulesDo: blk].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToCompile = ( |
            | 
            "By default, compile all mapped slots."
            modulesToMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhichShouldIgnoreTransporterLink = bootstrap setObjectAnnotationOf: ( set copyRemoveAll) From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy namesOfModulesWhichShouldIgnoreTransporterLink.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldAlwaysMapSlotNoMatterWhatModuleItIsIn: s = ( |
            | 
            s holder isReflecteeVMKitActivationMap ifTrue: [
              [codes. literals]. "browsing"
                 (s name = 'codes')
              || [s name = 'literals'] ifTrue: [^ true].

              shouldOmitActivationPartSizes ifFalse: [
               [activationPartSizes]. "browsing"
                s name = 'activationPartSizes' ifTrue: [^ true].
              ].

              shouldOmitSourceStrings ifFalse: [
                [sourceString. sourceOffset. sourceLength]. "browsing"
                    (s name = 'sourceString')
                || [(s name = 'sourceOffset')
                || [(s name = 'sourceLength') ]] ifTrue: [^ true].
              ].
            ].

            "Needed for proper functioning of the object table."
            s name = 'lastInvalidEntry' ifTrue: [^ true].

            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockValueSlotsBeObjectSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldCanonicalizeEmptyVectors = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldCompileInDebugMode = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldEviscerateModuleObjects = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeKleinPrimitives = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldKleinifyMirrors = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldNeverMapSlotNoMatterWhatModuleItIsIn: s = ( |
            | 
            s holder isReflecteeVMKitActivationMap ifTrue: [
              shouldOmitObjectAnnotations ifTrue: [
                [annotation. annotation: '']. "browsing"
                (s name = 'annotation') || [s name = 'annotation:'] ifTrue: [^ true].
              ].

              shouldOmitLexicalParent ifTrue: [
                [lexicalParent. lexicalParent: nil]. "browsing"
                (s name = 'lexicalParent') || [s name = 'lexicalParent:'] ifTrue: [^ true].
              ].

              shouldOmitActivationPartSizes ifTrue: [
                [activationPartSizes. activationPartSizes: 0]. "browsing"
                (s name = 'activationPartSizes') || [s name = 'activationPartSizes:'] ifTrue: [^ true].
              ].

              shouldOmitSourceStrings ifTrue: [
                [sourceString. sourceString: '']. "browsing"
                [sourceOffset. sourceOffset: 0 ]. "browsing"
                [sourceLength. sourceLength: 0 ]. "browsing"
                (s name = 'sourceString') || [s name = 'sourceString:'] ifTrue: [^ true].
                (s name = 'sourceOffset') || [s name = 'sourceOffset:'] ifTrue: [^ true].
                (s name = 'sourceLength') || [s name = 'sourceLength:'] ifTrue: [^ true].
              ].

              shouldOmitLineAndFile ifTrue: [
                [file. file: '']. "browsing"
                [line. line: 0 ]. "browsing"
                (s name = 'file') || [s name = 'file:'] ifTrue: [^ true].
                (s name = 'line') || [s name = 'line:'] ifTrue: [^ true].
              ].
            ].

            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitActivationPartSizes = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitAnnotations = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLexicalParent = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLineAndFile = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitObjectAnnotations = ( |
            | 
            "I'm guessing we'll almost always want to either include
             both or omit both slot annotations and object annotations,
             but we might as well have the option to omit one and
             include the other. -- Adam , 11/05"
            shouldOmitAnnotations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitSlotAnnotations = ( |
            | 
            "I'm guessing we'll almost always want to either include
             both or omit both slot annotations and object annotations,
             but we might as well have the option to omit one and
             include the other. -- Adam , 11/05"
            shouldOmitAnnotations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitSourceStrings = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOnlyCompileSelectorsCalledByMappedMethods = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Comment: We will need to make a new kind of slotFinder
when the time comes to get reflection working,
because we\'ll want one that respects this object\'s
isSlotToBeMapped: policy. -- Adam, 3/04\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         slotFinder = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'visited' From:
             traits mirrors abstractMirror slotFinder copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy slotFinder.

CopyDowns:
traits mirrors abstractMirror slotFinder. copy 
SlotsToOmit: parent visited.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy slotFinder parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> () From: ( | {
         'Category: finding slots\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         comparisonTraitsThatCanCompareMapsAndMirrors = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> 'comparisonTraitsThatCanCompareMapsAndMirrors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy slotFinder parent comparisonTraitsThatCanCompareMapsAndMirrors.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> 'comparisonTraitsThatCanCompareMapsAndMirrors' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         hashElement: e = ( |
            | 
            e hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> 'comparisonTraitsThatCanCompareMapsAndMirrors' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         is: e1 EqualTo: e2 = ( |
            | 
            "This seems like an awful hack. The problem is that we
             sometimes (e.g. for compilation) do slot lookup starting
             from a map rather than a mirror. -- Adam, May. 2009"

            ((reflect: e1) isReflecteeVMKitMap = (reflect: e2) isReflecteeVMKitMap) && [e1 = e2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> 'comparisonTraitsThatCanCompareMapsAndMirrors' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> () From: ( | {
         'Category: filtering out slots\x7fComment: The normal Self lookup algorithm is interested in every slot,
but children can override this to allow some slots to be
filtered out.\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         isInterestedInSlot: s = ( |
            | 
            theVM exportPolicy isSlotToBeMapped: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> 'slotFinder' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'slotFinder' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: InitializeToExpression: (customizableSet copy comparisonTraits: kleinAndYoda virtualMachines abstractVM exportPolicy slotFinder comparisonTraitsThatCanCompareMapsAndMirrors)\x7fVisibility: private'
        
         visited <- customizableSet copy comparisonTraits: kleinAndYoda virtualMachines abstractVM exportPolicy slotFinder comparisonTraitsThatCanCompareMapsAndMirrors.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         slotTypeOf: aSlot = ( |
             t.
            | 
            t: kleinAndYoda slotType fromSlot: aSlot.
            shouldBlockValueSlotsBeObjectSlots && [aSlot isBlockValueSlot]
              ifFalse: [t]
                 True: [kleinAndYoda slotType asObjectSlotType: t]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsToCompileForReceiver: rMir = ( |
             r.
            | 
            r: list copyRemoveAll.
            slotsToCompileForReceiver: rMir Do: [|:s| r add: s].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsToCompileForReceiver: rMir Do: blk = ( |
             slotNamesAlreadySeen.
            | 

            (isOkayToCompileMethodsForReflecteeOf: rMir) ifFalse: [^ self].

            slotNamesAlreadySeen: orderedSet copyRemoveAll.
            rMir slotsInMeAndAncestorsUpTo: (reflect: ()) Do: [|:slot. n|
              n: slot name.
              (slotNamesAlreadySeen includes: n) ifFalse: [
                (isSlotToBeCompiled: slot) ifTrue: [
                  slotNamesAlreadySeen add: n.
                  blk value: slot.
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         startingPoints = ( |
            | 
            shouldIncludeKleinPrimitives
              ifTrue: [(vmToExport & theVM vmKit primitives) asVector]
               False: [vector copyAddLast: vmToExport]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fComment: The VM being exported.\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         vmToExport = ( |
            | 
            theVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Objects that are not complete, but for which we would like to
compile slots.\x7fModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownIncompleteObjectsWithSlotsToCompile = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot'
        
         kleinExport = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinExport.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.64 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExport' -> () From: ( | {
         'ModuleInfo: Module: kleinExport InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinAsmPgmExporter
kleinVMExporter
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinAsmPgmExporter' From: 'applications/klein'
 bootstrap read: 'kleinVMExporter' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinExport postFileIn
