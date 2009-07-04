 '$Revision: 30.11 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> () From: ( | {
         'Category: Klein and Yoda virtual machine\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parentWithTheTheVMMethod* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'defaultBehavior' -> 'parentWithTheTheVMMethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: defaultBehavior parentWithTheTheVMMethod.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> 'parentWithTheTheVMMethod' -> () From: ( | {
         'Comment: Captures requests for the Klein Virtual Machine base
object during import and export and forwards them to a
process local.  While the Klein VM itself is running,
forwards them to a primitive.

This trick will not work when exporting another Klein VM
from within the running Klein VM because we would
want to use the process local for referring to the
VM to be exported, but also use the primitive
for accessing VM internals. -- jb 6/03\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         theVM = ( |
            | 
            _TheVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> () From: ( | {
         'Category: unmapped Klein and Yoda slots\x7fComment: Captures requests for the Klein Virtual Machine base
object during import and export and forwards them to a
process local.  While the Klein VM itself is running,
forwards them to a primitive.

This trick will not work when exporting another Klein VM
from within the running Klein VM because we would
want to use the process local for referring to the
VM to be exported, but also use the primitive
for accessing VM internals. -- jb 6/03\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         theVM = ( |
            | 
            process this theVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: oid-oop conversion\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop InVM: vm IfAbsent: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: oid-oop conversion\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid InVM: vm IfAbsent: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: oid-oop conversion\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop InVM: vm IfAbsent: fb = ( |
            | 
            vm oidForLocalOop: oop IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: oid-oop conversion\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid InVM: vm IfAbsent: fb = ( |
            | 
            vm localOopForOID: oid IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: oid-oop conversion\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop InVM: vm IfAbsent: fb = ( |
            | 
            vm oidForRemoteOop: oop IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: oid-oop conversion\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid InVM: vm IfAbsent: fb = ( |
            | 
            vm remoteOopForOID: oid IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: virtual machines\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         virtualMachines = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractVM = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)'
        
         aaa.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         architecture <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         image.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         lastUsedIdentityHash <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         lens.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)'
        
         lensSema.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         machineMemory.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         mirrorCache.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: boolean flags\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         nakedMethods <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         objectAnnotationStringCache.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         objectLocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( |
             {} = 'Comment: My children are virtual machines that are designed to be
mapped, exported, and then launched in a foreignProcess.
When executes ``over there\'\', they acquire complete
control over their environment.

An instance may be selected in scope by sending it the
message ``setTheVMAndDo:\'\' with a block.  Within that scope,
the instance may be accessed by sending ``theVM\'\' to
defaultBehavior.  Within the running VM, the VM instance
is always globally scoped.

-- jb 6/03\x7fModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: heap size\x7fComment: Allocates a new heap from machineMemory of the
specified size.  Must be called exactly once before
objects are allocated within the space.\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateHeapAt: base Size: s = ( |
            | 
            [todo optimize gc multipleSpaces]. "What's the right way to divide the space? -- Adam, 5/06"

            universe allocateHeapAt: base
                         NewGenSize: (canCollectGarbage ifTrue: [s / 16] False: [s / 8])  "leave room so we don't run out since no GC"
                  ScavengeSpaceSize: (canCollectGarbage ifTrue: [s /  4] False: 0      )  "scavenger creates a lot of garbage for now"
                          TotalSize: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: assertions\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         assert: blk = ( |
            | ifCheckAssertions: [blk assert: 'VM assertion failed']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: accessing platform information\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVectorLayout = ( |
            | 
            layouts segregatedByteVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: compiling\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         compilerPrototype = ( |
            | 
            vmKit compiler1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: copying\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (((((resend.copy
               universe: universe copy)
               objectLocator: objectLocator copy)
               lensSema: recursiveSemaphore copyBinary)
                 slotAnnotationStringCache: vmKit annotationStringCache copy)
               objectAnnotationStringCache: vmKit annotationStringCache copy)
               initializeMirrorCache).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: copying\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForArchitecture: archName = ( |
            | copy initializeForArchitecture: archName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: copying\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLaunchingImage: vmImage = ( |
             newVM.
            | 
            image statusReporter show: 'copying for launch'
            While: [
              newVM: resend.copy initializeMirrorCache.
              newVM objectLocator: objectLocator copy.
              newVM lensSema: recursiveSemaphore copyBinary.
              image willNeedToRelocate ifTrue: [
                newVM machineMemory: machineMemory copy.
              ].
              newVM image: vmImage.  vmImage myVM: newVM.

              image willNeedToRelocate ifTrue: [
                 newVM setTheVMAndDo: [ newVM universe: universe copyForLaunch ].
              ].
              vmImage reassociateKleinObjectFrom:          self To: newVM.
              vmImage reassociateKleinObjectFrom: objectLocator To: newVM objectLocator.
            ].
            newVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: copying\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         copyToOptimizeWordAccesses = ( |
            | 
            resend.copy initializeToOptimizeWordAccessesFrom: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: updating\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotsToFixUpIn: vmMir Do: blk = ( |
            | 
            objectLocator dataSlotsToFixUpIn: (vmMir primitiveContentsAt: 'objectLocator') Do: blk.
            universe      dataSlotsToFixUpIn: (vmMir primitiveContentsAt: 'universe'     ) Do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: startup\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         entryMethodName = ( |
            | 
            [startAfterSettingTheLens]. "browsing"
            'startAfterSettingTheLens').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: heap size\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 100000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: updating\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         exportHeapInformationTo: vmMir = ( |
            | 
            dataSlotsToFixUpIn: vmMir Do: [|:s. :localHolder|
              s setRemoteContentsFromLocalContentsIn: localHolder.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: exporting\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent exportPolicy.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         getANewOID = ( |
            | 
            withoutCloningAnythingGetANewOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: accessing platform information\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         hostName = ( |
            | 
            machineMemory hostName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: assertions\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         ifCheckAssertions: blk = ( |
            | 
            shouldCheckAssertions ifTrue: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: updating\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         importHeapInformationFrom: vmMir = ( |
            | 
            dataSlotsToFixUpIn: vmMir Do: [|:s. :localHolder|
              s fromRemoteContentsSetLocalContentsIn: localHolder.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: initializing\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForArchitecture: archName = ( |
            | 
            lens: vmKit localObjectLens.
            machineMemory: nil. "unused localMemoryInterface"
            universe: vmKit universe copyForVM: self.
            architecture: archName canonicalize.
            exportPolicy invalidateCachedModuleNameLists.
            objectLocator: objectLocatorProto copy.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: initializing\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeMirrorCache = ( |
            | 
            mirrorCache: vmKit mirrorCache copyForVM: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         lensForExecution = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         localMirrorFor: o IfFail: fb = ( |
            | 
            noncachingMirrorFor: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         localOopForOID: oid IfAbsent: fb = ( |
            | 
            objectLocator oopForOID: oid IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         machineMemoryForExecution = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'local' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         mapProtoForOop: oop IfFail: fb = ( |
            | 
            vmKit layouts object
              if:          oop
              IsImmediate: [|:layout| layout mapPrototype]
              IsMark:      [fb value: 'attempted to import a mark']
              IsMem:       [vmKit maps map mapProtoForMemMapOop: (mapOf: oop IfFail: [|:e| ^ fb value: e]) IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: menuing\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         menuOrder = ( |
            | 
            expectedNumberOfObjects).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorFor: o = ( |
            | 
            mirrorFor: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorFor: o IfFail: fb = ( |
            | 
            lens mirrorForVM: self Obj: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypes = ( |
            | 
            vmKit mirrors).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: accessing platform information\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            architecture sendTo: assemblerSystems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         nextFreeOID = ( |
            | 
            objectLocator nextFreeOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: identity hashes\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         nextIdentityHash = ( |
            | 
            lastUsedIdentityHash: lastUsedIdentityHash _IntAdd: 1 IfFail: [|:e. :p| 1].
            lastUsedIdentityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         noncachingMirrorFor: o = ( |
            | 
            noncachingMirrorFor: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         noncachingMirrorFor: o IfFail: fb = ( |
            | 
            setTheVMAndDo: [| mapProto. mirrorProto |
              mapProto: mapProtoForOop: o IfFail: [|:e| ^ fb value: e].
              mirrorProto: mapProto mirrorPrototypeFromNamespace: vmKit mirrors.
              mirrorProto copyForVM: self Oop: o IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: initializing\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         objectLocatorProto = ( |
            | 
            vmKit indirectPointerObjectLocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForLocalOop: oop IfAbsent: fb = ( |
            | 
            layouts memoryObject oidOf: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop IfAbsent: fb = ( |
            | 
            objectLocator oidForOop: oop IfNontrivial: [
              lens oidForOop: oop InVM: self IfAbsent: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForRemoteOop: oop IfAbsent: fb = ( |
            | 
            image oidForOop: oop IfAbsent: [
              layouts memoryObject oidOf: oop IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: fb = ( |
            | 
            lens oopForOID: oid InVM: self IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         overriddenByUnmappedSlots* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'overriddenByUnmappedSlots' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent overriddenByUnmappedSlots.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'overriddenByUnmappedSlots' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         setTheVMAndDo: blk = ( |
            | 
            "theVM is always set when I am the running VM"
            blk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: compiling\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         placeholderSelectorForDynamicPerform = ( |
            | 
            [todo cleanup perform].
            "Is there a better place for this, or a better way to accomplish
             the same thing? This string needs to be mapped if we compile any
             dynamic _Performs, so I put it in here so that it'll always be
             mapped. A better way might be to have the compiler (upon first
             encountering a dynamic _Perform) tell the objectMapper to map
             this string, but I don't think it's easy for the compiler to 
             talk to the mapper. -- Adam, 12/05"

            'thisSelectorShouldHaveGottenPatched').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: launching\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         pointMemoryInterfaceTo: aForeignProcess = ( |
            | 
            machineMemory: 
              vmKit memoryInterfaces foreignProcess
                copyForForeignProcess: aForeignProcess.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: mirrors\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMirrorFor: oop IfFail: fb = ( |
            | 
            mirrorCache importedMirrorFor: oop IfAbsentPut: [| m |
              m: noncachingMirrorFor: oop IfFail: [|:e| ^ fb value: e].
              m reflectionPrimitives:
                  vmKit cachingReflectionPrimitives
                    copyForNoncachingReflectionPrimitives: m reflectionPrimitives.
              m
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteOopForOID: oid IfAbsent: fb = ( |
            | 
            image oopForOID: oid IfAbsent: [^ fb value: 'Could not find oop for reflectee']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: theVM\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         setTheVMAndDo: blk = ( |
            | 
            process this setTheVM: self AndDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: setting trailing mark\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkIn: aSpace = ( |
            | 
            aSpace setTrailingMark.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: startup\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: startup\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         startAfterSettingTheLens = ( |
            | 
            _NoMapTest. "When we call this method to start the VM, there's
                         no calling sendDesc. But there's gotta be a solution
                         that'll let that work but also let us have a map
                         test if we ever call this module during normal
                         execution. -- Adam, 5/04"


            lens:                   lensForExecution. "lens gets munged during export"
            machineMemory: machineMemoryForExecution.

            start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: printing\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            architecture = '' ifTrue: [^ ''].
            'on ', architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: canonical strings\x7fComment: Can be used as the comparisonTraits for a customizableSet
or customizableDictionary if you want to make sure it
compares the contents of the byte vectors (rather than
possibly taking shortcuts like canonicalStrings do).
-- Adam, 6/04\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         stringComparisonMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'stringComparisonMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda virtualMachines abstractVM parent stringComparisonMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'stringComparisonMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         hashElement: bv = ( |
             end.
             h <- 0.
             i <- 0.
             n.
            | 
            n: bv size.
            h: n.
            end: n - 3.
            [ |:exit|
              i <= end ifFalse: exit.
              h: h ^^  (bv byteAt: i    )        hash
                   ^^ ((bv byteAt: i + 1) <<  8) hash
                   ^^ ((bv byteAt: i + 2) << 16) hash.
              i: i + 3.
            ] loopExit.
             i      < n ifFalse: [^ h].  h: h ^^  (bv byteAt: i    )        hash.
            (i + 1) < n ifFalse: [^ h].  h: h ^^ ((bv byteAt: i + 1) <<  8) hash.

            h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'stringComparisonMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         is: e1 EqualTo: e2 = ( |
            | 0 = (e1 byteVectorCompare: e2 IfFail: -1)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: incrementally updating\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOop: oldOop ToHaveAddress: newAddr = ( |
            | 
            objectLocator switchPointersFromObjectWithOop: oldOop ToHaveAddress: newAddr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: error handling\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitsError: message = ( |
            | 
            'error: ' _StringPrint.
            message _StringPrint.
            '\n'_StringPrint.
            'called error:' _Breakpoint.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: primitive failure\x7fComment: todo yoda primitives cleanup
This is just a temporary hack, needed until yoda has 
more functionality.
Ideally, yoda should work just like regular Self. Remove
this message, or make it yoda specific. ~Ausch\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitsPrimitiveFailedError: error Name: sel = ( |
            | 
            'Primitive failed, primitive has no fail block. Error Name: ' _StringPrint.
            error _StringPrint.
            '  Selector: ' _StringPrint.
            sel _StringPrint.
            '\n' _StringPrint.
            'primitive failed' _Breakpoint.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         withoutCloningAnythingGetANewOID = ( |
            | 
            objectLocator withoutCloningAnythingAllocateAnOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: object IDs\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         withoutCloningAnythingRecordNewObjectWithAddress: addr IfFail: fb = ( |
             oid.
            | 
            oid: withoutCloningAnythingGetANewOID.
            objectLocator withoutCloningAnythingRecordAddress: addr ForOID: oid.
            oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         shouldCheckAssertions <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         slotAnnotationStringCache.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot'
        
         startSelector = 'startAfterSettingTheVMAndLens'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: \"global\" state\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         universe.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> () From: ( | {
         'Category: boolean flags\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         wizardMode <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot'
        
         vmKitVM = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVM.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVM' -> () From: ( | {
         'ModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'process' -> () From: ( | {
         'Category: per-process globals\x7fCategory: Klein virtual machine\x7fComment: For importing and exporting objects to the
Klein Virtual Machine. Someday, will be used for the VM
itself. -- dmu 5/02\x7fModuleInfo: Module: vmKitVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         myVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'process' -> () From: ( | {
         'Category: Klein VM export and debugging\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         setTheVM: vm AndDo: blk = ( |
             oldVM.
            | 
            oldVM: process this myVM.
            process this myVM: vm.
            blk onReturn: [process this myVM: oldVM]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'process' -> () From: ( | {
         'Category: Klein VM export and debugging\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         theVM = ( |
            | 
            theVMIfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'process' -> () From: ( | {
         'Category: Klein VM export and debugging\x7fModuleInfo: Module: vmKitVM InitialContents: FollowSlot\x7fVisibility: public'
        
         theVMIfAbsent: blk = ( |
            | 
            myVM ifNil: [blk value: 'no VM']).
        } | ) 



 '-- Side effects'

 globals modules vmKitVM postFileIn
