 '$Revision: 30.38 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         relocationInfo.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot'
        
         relocators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         compiledOop.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         offset <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         originalObject.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocationInfoTo: s = ( |
            | 
            s add: prototype.
            s add: compiledOop.
            s add: originalObject.
            s add: offset.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         assembleRealOrPlaceholderInstructionsWith: cg = ( |
            | 
            [todo optimize]. "Should there be maybe one big list of relocators (or, rather, indices
                              into relocationInfo vectors) that have placeholder instructions, so that
                              we don't need to enumerate it when we do the final relocation? I don't
                              think this is costing us much time now - the Image Initialization
                              phase is only about 5 seconds, compared to about 300 for the Mapping
                              phase. But keep in mind as an idea for later. -- Adam, 6/05"
            vmKit relocators isEagerRelocationEnabled ifFalse: [^ assemblePlaceholderInstructionsWith: cg].

            initializeCompiledOop: cg compiler oracleForEagerRelocation IfFail: [
              cg compiler oracleForEagerRelocation relocatorAssembledPlaceholderInstructions.
              ^ assemblePlaceholderInstructionsWith: cg
            ].
            cg compiler oracleForEagerRelocation relocatorAssembledRealInstructions.
            reassembleWith: cg NewOop: compiledOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: setting target\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         beForObject: o = ( |
            | originalObject: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOffset: off Object: o = ( |
            | 
            (copy
             offset: off)
             originalObject: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         hasCompiledOopBeenSet = ( |
            | 
            nil != compiledOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: setting target\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         if: oldMir IsFoundCopyAndReplaceWith: newMir IfFound: blk = ( |
            | 
            (reflect: originalObject) = oldMir ifTrue: [
              blk value: copy beForObject: newMir reflectee
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         initialAddress = -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: linearizing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeCompiledOop: oracle IfFail: fb = ( |
            | 
            compiledOop:  oracle oopForOriginalObject: originalObject IfAbsent: [^ fb value].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: manipulating the oop in the nmethod\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         insertOopInNMethod: nmOop Using: oracle With: cg = ( |
            | 
            initializeCompiledOop: oracle IfFail: raiseError.
            relocateOopInNMethod: nmOop To: compiledOop With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         isLoadingTheNMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         reconstructFromRelocationInfo: s = ( |
            | 
               compiledOop: s removeFirst.
            originalObject: s removeFirst.
                    offset: s removeFirst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: manipulating the oop in the nmethod\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateOopInNMethod: nmOop By: delta With: cg = ( |
            | 
            relocateOopInNMethod: nmOop To: compiledOop + delta With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: manipulating the oop in the nmethod\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         relocateOopInNMethod: nmOop To: newOop With: cg = ( |
            | 
            reassembleWith: cg NewOop: newOop.
            writeBytesInNMethod: nmOop With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: manipulating the oop in the nmethod\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         writeBytesInNMethod: nmOop With: cg = ( |
             codeAddr.
            | 
            codeAddr: vmKit maps nmethodMap for: nmOop AddressOfIndexableAt: offset.
            theVM machineMemory at: codeAddr PutBytes: cg a assembledBytes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         data = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein relocators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators data.

CopyDowns:
globals klein relocators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators data parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblePlaceholderInstructionsWith: cg = ( |
            | 
            cg a data32: initialAddress.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            klein relocators data).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'data' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         reassembleWith: cg NewOop: newOop = ( |
            | 
            cg a data32:  theVM lens oopOfMem: newOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'Category: eager relocation\x7fComment: Things to remove if we take this out (as well as
all the calls to this method, of course):
  cachedKleinPrimitivesNMethods
  eagerNMethodLoadingRelocationTime
  eagerNMethodLoadingOptimismJustified
  eagerNMethodLoadingOptimismNotJustified
  relocatorNeedsToDoDummyInstructions
  relocatorDoesNotNeedToDoDummyInstructions
  noLinearizedObjects
  linearizedObjects
  linearizedObjectsForEagerRelocation
  maybe the loadNMethod kind of relocator\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         isEagerRelocationEnabled = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         loadAddress = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein relocators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators loadAddress.

CopyDowns:
globals klein relocators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         dstReg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators loadAddress parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocationInfoTo: s = ( |
            | 
            resend.addRelocationInfoTo: s.
            s add: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblePlaceholderInstructionsWith: cg = ( |
            | 
            cg a loadAddressTo: dstReg From: initialAddress.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDstReg: r Offset: off Object: o = ( |
            | 
            (copyOffset: off Object: o)
             dstReg: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            klein relocators loadAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         reassembleWith: cg NewOop: newOop = ( |
            | 
            cg a loadAddressTo: dstReg
                          From: theVM lens oopOfMem: newOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         reconstructFromRelocationInfo: s = ( |
            | 
            resend.reconstructFromRelocationInfo: s.
            dstReg: s removeFirst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         loadNMethod = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein relocators loadAddress copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators loadNMethod.

CopyDowns:
globals klein relocators loadAddress. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators loadNMethod parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblePlaceholderInstructionsWith: cg = ( |
            | 
            vmKit relocators isEagerRelocationEnabled ifFalse: [^ resend.assemblePlaceholderInstructionsWith: cg].

            "Optimistically assume that the nmethod will be the next object we allocate.
             We have to guess the size of the nmethod; no big deal if we get it wrong,
             except that we might have to tell this relocator to reassemble later. -- Adam, 5/06"
            cg a loadAddressTo: dstReg From: cg compiler oracleForEagerRelocation nextOopToAllocateForObjectOfSize: 350.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         isLoadingTheNMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadNMethod' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            klein relocators loadNMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         objectSlotOffset = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein relocators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators objectSlotOffset.

CopyDowns:
globals klein relocators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)'
        
         baseReg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)'
        
         dataReg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)'
        
         holderSelfObj.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (nil)'
        
         isLoad.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (\'\')'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators objectSlotOffset parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocationInfoTo: s = ( |
            | 
            resend.addRelocationInfoTo: s.
            s add: baseReg.
            s add: dataReg.
            s add: holderSelfObj.
            s add: isLoad.
            s add: name.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblePlaceholderInstructionsWith: cg = ( |
            | 
            assembleWith: cg
                Location: cg locations offsetFromOtherLocation copyForOffset: initialOffset FromRegister: baseReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         assembleWith: cg Location: loc = ( |
            | 
            isLoad ifTrue: [ cg moveLocation: loc     ToRegister: dataReg ]
                    False: [ cg moveRegister: dataReg ToLocation: loc     ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOffset: off Slot: s DataReg: dr BaseReg: br IsLoad: isL = ( |
            | 
            (((((
             copyOffset: off Object: -1)
             holderSelfObj: s holder reflectee)
             name: s name)
             dataReg: dr)
             baseReg: br)
             isLoad: isL).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         dataSlotName = ( |
            | 
            isLoad ifTrue: [name] False: [name copyWithoutLast]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         initialOffset = -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: linearizing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeCompiledOop: oracle IfFail: fb = ( |
             map.
             objectOop.
            | 
            objectOop:   oracle oopForOriginalObject: holderSelfObj  IfAbsent: [^ fb value].
            map:         vmKit maps map importMapFor: objectOop      IfFail:   [^ fb value].
            slotOffset:  map mapDataOopOfSlotNamed: dataSlotName     IfAbsent: [^ fb value].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            klein relocators objectSlotOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         reassembleWith: cg NewOop: newOop = ( |
             memoryOffset.
            | 
            memoryOffset: slotOffset * vmKit base oopSize.
            assembleWith: cg
                Location: cg locations offsetFromOtherLocation copyForOffset: memoryOffset FromRegister: baseReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         reconstructFromRelocationInfo: s = ( |
            | 
            resend.reconstructFromRelocationInfo: s.
            baseReg:       s removeFirst.
            dataReg:       s removeFirst.
            holderSelfObj: s removeFirst.
            isLoad:        s removeFirst.
            name:          s removeFirst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: manipulating the oop in the nmethod\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateOopInNMethod: nm By: delta With: cg = ( |
            | 
            self "offsets do not changes when objects move").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         slotOffset = ( |
            | 
            "miniVM isSlotToBeMapped: wants the actual data slot to
             be named compiledOop, but for this kind of relocator
             the compiledOop slot is really the offset of the slot
             to be accessed. (We could change compiledOop to
             slotOffset and then just tell isSlotToBeMapped: to
             also map slots named slotOffset. I don't know whether
             that's worth doing or not.) -- Adam, 3/05"
            compiledOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'objectSlotOffset' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         slotOffset: o = ( |
            | 
            compiledOop: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         stub = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein relocators loadAddress copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators stub.

CopyDowns:
globals klein relocators loadAddress. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         methodName.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein relocators stub parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocationInfoTo: s = ( |
            | 
            resend.addRelocationInfoTo: s.
            s add: methodName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblePlaceholderInstructionsWith: cg = ( |
            | 
            cg a loadAddressTo: dstReg
                          From: initialAddress.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDstReg: r Offset: off MethodNamed: mn = ( |
            | 
            ((copy 
              offset: off)
              methodName: mn)
              dstReg: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'Category: linearizing\x7fComment: During export, the nmethod oop is not available yet.
So, just store the nmethod name until the oop is needed.
-- dmu 3/04\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeCompiledOop: oracle IfFail: fb = ( |
            | 
            oracle
              findOopForStubNMethodNamed: methodName
                               IfPresent: [|:nmOop|
                                           vmKit relocators isEagerRelocationEnabled ifTrue: [
                                             "Make sure it has an entry address. (If we're not doing
                                              eager relocation then it'll always have an entry address
                                              because we don't do the relocation until the end.)"
                                             vmKit maps nmethodMap entryAddressOf: nmOop IfFail: [^ fb value].
                                           ].
                                           compiledOop: nmOop]
                                IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'loadAddress' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            klein relocators stub).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         reassembleWith: cg NewOop: newOop = ( |
            | 
            cg a loadAddressTo: dstReg
                          From: vmKit maps nmethodMap entryAddressOf: newOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'relocators' -> 'stub' -> 'parent' -> () From: ( | {
         'Category: flattened relocation info\x7fModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         reconstructFromRelocationInfo: s = ( |
            | 
            resend.reconstructFromRelocationInfo: s.
            methodName: s removeFirst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot'
        
         kleinRelocators = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinRelocators.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.38 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinRelocators' -> () From: ( | {
         'ModuleInfo: Module: kleinRelocators InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinRelocators postFileIn
