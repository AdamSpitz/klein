 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot'
        
         yodaLayouts = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaLayouts.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaLayouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'yodaVarHdrs
yodaWellKnowns
yodaActivation
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces, also C structures\x7fModuleInfo: Module: yodaLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         layouts = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda layouts.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> () From: ( | {
         'ModuleInfo: Module: yodaLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> ().
        } | ) 



 '-- Sub parts'

 bootstrap read: 'yodaVarHdrs' From: 'applications/klein'
 bootstrap read: 'yodaWellKnowns' From: 'applications/klein'
 bootstrap read: 'yodaActivation' From: 'applications/klein'



 '-- Side effects'

 globals modules yodaLayouts postFileIn
