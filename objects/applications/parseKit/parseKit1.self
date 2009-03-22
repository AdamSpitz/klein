 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot'
        
         parseKit1 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKit1.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
             resend.postFileIn.
            parseKit lexer tokens longestMatch initializePrototype.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit1' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         eof = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'eof' -> () ToBe: bootstrap addSlotsTo: (
             globals parseKit lexer tokens token copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'eof' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokens eof.

CopyDowns:
globals parseKit lexer tokens token. copy

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'eof' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isLast = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'eof' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         scanIfFail: fb = ( |
            | 
            setEnd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'eof' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = 'EOF'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         longestMatch = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'initialCharacterSet' From:
             bootstrap remove: 'parent' From:
             globals parseKit lexer tokens token copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> () From: ( |
             {} = 'Comment: Invented for Java operators, 
I am a general token that matches
the longest matching element in \"legalOperators\"
where legalOperators is just a set of strings.
Send initializePrototype to me when my module is filed in.\x7fModuleInfo: Creator: globals parseKit lexer tokens longestMatch.

CopyDowns:
globals parseKit lexer tokens token. copy 
SlotsToOmit: initialCharacterSet parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> () From: ( | {
         'Category: set-up\x7fComment: a set of characters that are permitted
to start this kind of token\x7fModuleInfo: Module: parseKit1 InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: public'
        
         initialCharacterSet <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokens longestMatch parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         advancePastMeIfFail: fb = ( |
             src.
            | 
            "lexer in points to first char of me.
             Find longest legal operator."
            src: in peekAtMost: maxSize.
            maxSize downTo: 1 Do: [|:s|
              ((sortedMatches at: s pred) includes: src)
                ifTrue: [ in advance: s.  ^ self ].
              src: src copySize: s pred.
            ].
            setStart.
            illegalTokenReason: 'illegal operator' FailBlock: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'Category: set-up\x7fModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initInitialCharacterSet = ( |
             r.
            | 
            r: sortedMatches first asString asCharacterSet.
            initialCharacterSet: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'Category: set-up\x7fModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initSortedMatches = ( |
             r.
            | 
            r: vector copyRemoveAll.
            legalOperators do: [|:op|
              "r at: 0 holds size: 1 ops, so expand if needed"
              op size  >  r size  ifTrue: [ | rr |
                rr: r copySize: op size.
                r size upTo: rr size Do: [|:i| rr at: i Put: set copyRemoveAll ].
                r: rr.
              ].
              (r at: op size pred) add: op.
            ].
            sortedMatches: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'Category: set-up\x7fModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializePrototype = ( |
            | 
            initSortedMatches.
            initInitialCharacterSet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'Category: override these\x7fComment: This is just an example, override me.\x7fModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         legalOperators = bootstrap setObjectAnnotationOf: ( (('+')
	& ('++')
	& ('+=')
	& ('-')
	& ('--')
	& ('-=')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokens longestMatch parent legalOperators.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'Category: set-up\x7fModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: private'
        
         maxSize = ( |
            | 
            sortedMatches size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'longestMatch' -> () From: ( | {
         'Category: set-up\x7fComment: A vector of sets of matches
first element contains all 1-char matches,
second element contains all 2-char matches,
etc.\x7fModuleInfo: Module: parseKit1 InitialContents: InitializeToExpression: (vector copyRemoveAll)\x7fVisibility: private'
        
         sortedMatches <- vector copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         whiteSpace = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'whiteSpace' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'initialCharacterSet' From:
             bootstrap remove: 'initialCharacterSet:' From:
             globals parseKit lexer tokens token copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'whiteSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokens whiteSpace.

CopyDowns:
globals parseKit lexer tokens token. copy 
SlotsToOmit: initialCharacterSet initialCharacterSet:.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'whiteSpace' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         initialCharacterSet = ' 	

' asCharacterSet.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'whiteSpace' -> () From: ( | {
         'ModuleInfo: Module: parseKit1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isMeaningful = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 



 '-- Side effects'

 globals modules parseKit1 postFileIn
