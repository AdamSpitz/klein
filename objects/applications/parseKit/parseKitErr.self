 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot'
        
         parseKitErr = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitErr.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'parseKitErr1
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fCategory: errors\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         syntaxError = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit syntaxError.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: InitializeToExpression: (parseKit inputExtent)'
        
         extent <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: InitializeToExpression: (parseKit inputStream)'
        
         inStream <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit syntaxError parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         = err = ( |
            | 
            (reason = err reason) &&
            [(inStream = err inStream) &&
            [extent = err extent]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         asString = ( |
            | statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | 
            extent end asPoint - (0@1) "0 in x because UI2 selects upTo").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            reason hash ^^ extent hash ^^ inStream heash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         printString = ( |
            | 
            "override for ui2ResultReporter"
            statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         reason: r Input: in Extent: e = ( |
            | 
            ((copy  reason: r) inStream: in) extent: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         reason: r Source: s = ( |
             in.
            | 
            in: parseKit inputStream copyForString: s.
            reason: r Input: in Extent: parseKit inputExtent startAndEnd: in position).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         source = ( |
            | inStream sourceAt: extent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            extent start asPoint - (1@1)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: private'
        
         statePrintString = ( |
             src.
            | 
            src: inStream sourceAt: extent.
            reason, (src isEmpty ifTrue: '' False: [': ', src])).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitErr InitialContents: FollowSlot\x7fVisibility: public'
        
         testResultString = ( |
            | 
            reason, ' `', source, '\' at: ', extent testResultString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'syntaxError' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr InitialContents: InitializeToExpression: (\'proto error\')'
        
         reason <- 'proto error'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'parseKitErr1' From: 'applications/parseKit'



 '-- Side effects'

 globals modules parseKitErr postFileIn
