 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         parseKitSamples = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitSamples.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitSamples' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: samples\x7fModuleInfo: Module: parseKitSamples InitialContents: FollowSlot\x7fVisibility: public'
        
         sampleLexer = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'spaces' From:
             bootstrap remove: 'tokens' From:
             globals parseKit lexer copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit sampleLexer.

CopyDowns:
globals parseKit lexer. copy 
SlotsToOmit: spaces tokens.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         spaces = ' 
\x0d	'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot\x7fVisibility: public'
        
         test = ( |
             lxr.
             r.
            | 
            r: list copyRemoveAll.
            lxr: copyForInputStream: 
              inputStream copyForString: 'a + b - c'.
            [r addLast: lxr next] untilTrue: [r last isLast].
            r asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         tokens = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit sampleLexer tokens.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         operator = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> 'operator' -> () ToBe: bootstrap addSlotsTo: (
             globals parseKit lexer tokens token copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> 'operator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit sampleLexer tokens operator.

CopyDowns:
globals parseKit lexer tokens token. copy

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitSamples InitialContents: FollowSlot'
        
         shortIdentifier = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> 'shortIdentifier' -> () ToBe: bootstrap addSlotsTo: (
             globals parseKit lexer tokens token copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'sampleLexer' -> 'tokens' -> 'shortIdentifier' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit sampleLexer tokens shortIdentifier.

CopyDowns:
globals parseKit lexer tokens token. copy

\x7fIsComplete: '.
            | ) .
        } | ) 



 '-- Side effects'

 globals modules parseKitSamples postFileIn
