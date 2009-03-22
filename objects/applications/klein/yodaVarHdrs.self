 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot'
        
         yodaVarHdrs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaVarHdrs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockMapsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlocksBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableOrigin = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableSize = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMethodMapsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldStringsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldVectorsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockMapsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlocksBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableOrigin = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableSize = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMethodMapsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldStringsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: yodaVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldVectorsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 



 '-- Side effects'

 globals modules yodaVarHdrs postFileIn
