 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         javaCategoryReferrent = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals selfCategoryReferrent copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 javaCategoryReferrent.

CopyDowns:
globals selfCategoryReferrent. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 javaCategoryReferrent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         isClass = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         isInterface = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'javaCategoryReferrent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'selfCategoryReferrent' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         javaCatReferrent = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaCatReferrent.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaCatReferrent' -> () From: ( | {
         'ModuleInfo: Module: javaCatReferrent InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaCatReferrent postFileIn
