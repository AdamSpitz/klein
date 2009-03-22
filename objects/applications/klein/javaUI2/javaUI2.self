 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'generalModel' -> 'parent' -> () From: ( | {
         'Category: type testing\x7fCategory: Java\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaCategoryModel = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'generalModel' -> 'parent' -> () From: ( | {
         'Category: type testing\x7fCategory: Java\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaPseudoCategoryModel = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'generalModel' -> 'parent' -> () From: ( | {
         'Category: type testing\x7fCategory: Java\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaSlotModel = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'generalModel' -> 'parent' -> () From: ( | {
         'Category: type testing\x7fCategory: Java\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaTypeDclModel = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         javaUI2 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: morphs\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         compilationUnitAnnotationMorph = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals abstractAnnotationMorph copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitAnnotationMorph.

CopyDowns:
globals abstractAnnotationMorph. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myCompilationUnitOutliner.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () From: ( | {
         'Category: subparts\x7fModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (nil)'
        
         packageNameField.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitAnnotationMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptClassFileNameIfFail: fb = ( |
            | 
            sourceFileNameField hasChanged ifFalse: [^self].
            setClassFileNameIfFail: fb.
            sourceFileNameField clearChanges.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptModuleInfoIfFail: fb = ( |
            | 
            acceptClassFileNameIfFail: fb.
            acceptPackageNameIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptPackageNameIfFail: fb = ( |
            | 
            packageNameField hasChanged ifFalse: [^self].
            setPackageNameIfFail: fb.
            packageNameField clearChanges.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildPackageName = ( |
            | 
            packageNameField:
              moduleField copyLabel: 'Package'
                             String: compilationUnit packageName
                              Style: style
                             Target: self.
            contentsColumn addMorphLast: packageNameField.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildSourceFileName = ( |
            | 
            sourceFileNameField:
              moduleField copyLabel: 'Source file'
                             String: compilationUnit fileName
                              Style: style
                             Target: self.
            contentsColumn addMorphLast: sourceFileNameField.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         compilationUnit = ( |
            | 
            myCompilationUnitOutliner model compilationUnit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompilationUnitOutliner: co Style: sty = ( |
            | 
            ((copy myCompilationUnitOutliner: co) style: sty) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         getClassFileNameIfFail: fb = ( |
            | 
            sourceFileNameField editor contentsString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         getPackageNameIfFail: fb = ( |
            | 
            packageNameField editor contentsString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            buildSourceFileName.
            buildPackageName.
            colorAll: myCompilationUnitOutliner color.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'Java compilationUnitAnnotationMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'abstractAnnotationMorph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         receiver = ( |
            | 
            myCompilationUnitOutliner model receiver).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         setClassFileNameIfFail: fb = ( |
             n.
            | 
            n: getClassFileNameIfFail: fb.
            class fileName: n IfFail: [|:err|
              ^ fb value: 
                  syntaxErrorMorph copyTitle: 'Could not change class file name'
                                     Message: err
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         setPackageNameIfFail: fb = ( |
             n.
            | 
            n: getPackageNameIfFail: fb.
            class packageName: n IfFail: [|:err|
              ^ fb value:
                syntaxErrorMorph copyTitle: 'Could not change package name'
                                   Message: err
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         updateCompilationUnitFileName = ( |
            | 
            sourceFileNameField updateTo: compilationUnit fileName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         updateNoKidding = ( |
            | 
            updateCompilationUnitFileName.
            updatePackageName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         updatePackageName = ( |
            | 
            packageNameField updateTo: compilationUnit packageName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            javaUI2 compilationUnitAnnotationMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitAnnotationMorph' -> () From: ( | {
         'Category: subparts\x7fModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         sourceFileNameField.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         oldOnes* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         classAnnotationMorph = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals abstractAnnotationMorph copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes classAnnotationMorph.

CopyDowns:
globals abstractAnnotationMorph. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myClassOutliner.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () From: ( | {
         'Category: subparts\x7fModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (nil)'
        
         packageNameField.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes classAnnotationMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptClassFileNameIfFail: fb = ( |
            | 
            sourceFileNameField hasChanged ifFalse: [^self].
            setClassFileNameIfFail: fb.
            sourceFileNameField clearChanges.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptModuleInfoIfFail: fb = ( |
            | 
            acceptClassFileNameIfFail: fb.
            acceptPackageNameIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptPackageNameIfFail: fb = ( |
            | 
            packageNameField hasChanged ifFalse: [^self].
            setPackageNameIfFail: fb.
            packageNameField clearChanges.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildPackageName = ( |
            | 
            packageNameField:
              moduleField copyLabel: 'Package'
                             String: class packageName
                              Style: style
                             Target: self.
            contentsColumn addMorphLast: packageNameField.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildSourceFileName = ( |
            | 
            sourceFileNameField:
              moduleField copyLabel: 'Source file'
                             String: class fileName
                              Style: style
                             Target: self.
            contentsColumn addMorphLast: sourceFileNameField.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         class = ( |
            | myClassOutliner class).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyClassOutliner: co Style: sty = ( |
            | 
            ((copy myClassOutliner: co) style: sty) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         getClassFileNameIfFail: fb = ( |
            | 
            sourceFileNameField editor contentsString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         getPackageNameIfFail: fb = ( |
            | 
            packageNameField editor contentsString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            buildSourceFileName.
            buildPackageName.
            colorAll: myClassOutliner color.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'Java classAnnotationMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'abstractAnnotationMorph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         receiver = ( |
            | myClassOutliner receiver).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         setClassFileNameIfFail: fb = ( |
             n.
            | 
            n: getClassFileNameIfFail: fb.
            class fileName: n IfFail: [|:err|
              ^ fb value: 
                  syntaxErrorMorph copyTitle: 'Could not change class file name'
                                     Message: err
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: accepting module info\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         setPackageNameIfFail: fb = ( |
             n.
            | 
            n: getPackageNameIfFail: fb.
            class packageName: n IfFail: [|:err|
              ^ fb value:
                syntaxErrorMorph copyTitle: 'Could not change package name'
                                   Message: err
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         updateClassFileName = ( |
            | 
            sourceFileNameField updateTo: class fileName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         updateNoKidding = ( |
            | 
            updateClassFileName.
            updatePackageName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         updatePackageName = ( |
            | 
            packageNameField updateTo: class packageName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         prototype = ( |
            | javaUI2 classAnnotationMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classAnnotationMorph' -> () From: ( | {
         'Category: subparts\x7fModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         sourceFileNameField.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         classEnumerationMorph = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mirror' From:
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals mirrorEnumerationMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes classEnumerationMorph.

CopyDowns:
globals mirrorEnumerationMorph. copyRemoveAllMorphs 
SlotsToOmit: mirror parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (javaParser classOrInterface copy)'
        
         class <- javaParser classOrInterface copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes classEnumerationMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         classAt: index = ( |
            | 
            javaUI2 classOutliner 
              copyClass: (result at: index)
                  World: world).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyClass: cls = ( |
            | 
            (uninitializedCopy class: cls) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyClass: cls Event: evt = ( |
            | 
            (copyClass: cls) expand: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyMirror: mirr = ( |
            | irrelevant).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyMirror: mirr Event: evt = ( |
            | irrelevant).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         enumerationType = 'class enumeration'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         mirror = ( |
            | irrelevant).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'classEnumerationMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAt: index = ( |
            | irrelevant).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'mirrorEnumerationMorph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         titleString = ( |
            | 
            'Enumeration of ', class name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | enumerationMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         classReferencesMorph = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 oldOnes classEnumerationMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes classReferencesMorph.

CopyDowns:
globals javaUI2 oldOnes classEnumerationMorph. copyRemoveAllMorphs 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes classReferencesMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         action = ( |
            | class references).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         interestingPartOfObject: aClassOutliner At: index Event: evt = ( |
            | 
            aClassOutliner 
              unfold: (at: index) 
               Event: evt
              IfFail: [aClassOutliner]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'classReferencesMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         nameAt: index = ( |
            | 
            (result at: index) name, ' in ', (result at: index) holder name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAt: index = ( |
            | 
            classOutliner copyClass: (at: index) holder World: world).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classReferencesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         titleString = ( |
            | 
            'References to class ', class name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         java_textField = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals ui2_textField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes java_textField.

CopyDowns:
globals ui2_textField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes java_textField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> 'parent' -> () From: ( | {
         'Category: execution commands\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         do_it_cmd: evt = ( |
            | 
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> 'parent' -> () From: ( | {
         'Category: execution commands\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         get_it_cmd: evt = ( |
            | 
            "performed in traits ui2_textField keyDown:"
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         morphTypeName = 'java_textField'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'ui2_textField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         subclassesMorph = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 oldOnes classEnumerationMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes subclassesMorph.

CopyDowns:
globals javaUI2 oldOnes classEnumerationMorph. copyRemoveAllMorphs 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes subclassesMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         action = ( |
            | 
            class subclasses).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'subclassesMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'classEnumerationMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'subclassesMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         titleString = ( |
            | 'Subclasses of ', class name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         uglyTextEditorMorph = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals uglyTextEditorMorph copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes uglyTextEditorMorph.

CopyDowns:
globals uglyTextEditorMorph. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 oldOnes uglyTextEditorMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'javaUI2 uglyTextEditorMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'uglyTextEditorMorph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'uglyTextEditorMorph' -> 'parent' -> () From: ( | {
         'Category: for java\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         ui2_textField = bootstrap stub -> 'globals' -> 'javaUI2' -> 'oldOnes' -> 'java_textField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         sliceGroupModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalSliceGroupModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> () From: ( |
             {} = 'Comment: I represent a group of slots in a slice.
May contain subgroups. -- dmu 5/1\x7fModuleInfo: Creator: globals javaUI2 sliceGroupModel.

CopyDowns:
globals generalSliceGroupModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 sliceGroupModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         javaUI2Models* = bootstrap stub -> 'globals' -> 'javaUI2' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalSliceGroupModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'sliceGroupModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         titleString = ( |
            | 
            'in: ', mirror longKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         javaUI2 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
             resend.postFileIn.
            [todo cleanup. "rm next line someday"].
            worldMorph addBackgroundMenuContributor: javaUI2 compilationUnitModel.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2' -> () From: ( | {
         'ModuleInfo: Module: javaUI2 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'javaUI2CUModel
javaUI2ACModel
javaUI2ASModel
javaUI2ImpPkgModel
javaCatReferrent
javaMethodText
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'javaUI2CUModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2ACModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2ASModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2ImpPkgModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaCatReferrent' From: 'applications/klein/javaUI2'



 '-- Side effects'

 globals modules javaUI2 postFileIn
