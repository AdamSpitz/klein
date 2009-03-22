 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot'
        
         parseKitTokens = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitTokens.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokenTraits' -> 'valueTokens' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isLiteral = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         token = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes childless copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokens token.

CopyDowns:
globals parseKit parseNodes childless. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: InitializeToExpression: (parseKit inputExtent)\x7fVisibility: public'
        
         extent <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         in.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> () From: ( | {
         'Category: change these for types of tokens\x7fComment: a set of characters that are permitted
to start this kind of token\x7fModuleInfo: Module: parseKitTokens InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: public'
        
         initialCharacterSet <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( |
             {} = 'Comment: Many of my methods take a lexer as argument
when it would seem they should take something simpler,
such as the input stream.
This allows for more interesting overrides.\x7fModuleInfo: Creator: globals parseKit lexer tokens token parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fComment: Advance lexer to first character after me.\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         advancePastMeIfFail: fb = ( |
            | 
            in advance.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         basicTestResultString = ( |
            | statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: constructing\x7fComment: Called when the last character has been found.
(Input stream points to first character AFTER
this token.)
My job is to boil down the input characters of this
token as need be.
For instance, for an integer, I would calculate the
integer value from the characters.\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         condenseValueIfFail: fb = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: creating\x7fComment: Standard copy method,
only use \"copy\" to clone
a token that already has 
its lexer set.\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLexer: l = ( |
            | 
            (copy initForLexer: l) setStart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFrom: tok = ( |
            | 
            (copy extent: tok extent) in: tok in).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | extent end).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         finishIfFail: fb = ( |
            | 
            setEnd.
            condenseValueIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         firstToken = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         illegalTokenReason: msg FailBlock: fb = ( |
            | 
            setEnd.
            fb value: parseKit syntaxError reason: msg Input: in Extent: extent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         initForLexer: aLexer = ( |
            | 
            "default: just grab input stream"
            in: aLexer in).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isComment = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: testing\x7fComment: false for comments and whitespace
true for identifiers, operators, etc.\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isMeaningful = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isToken = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         lastToken = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fComment: Basic routine to scan this token.
On entry, the input stream is usually pointing at the first
character in this token, but we have already 
decided that this is the token to scan
(and already set the start of this token\'s extent).
On exit, the input stream points to the first character
after this token.\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         scanIfFail: fb = ( |
            | 
            (advancePastMeIfFail: [|:e| ^ fb value: e])
              finishIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: input position\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         setEnd = ( |
            | 
            extent: extent end: in previousPosition).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: input position\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         setStart = ( |
            | 
            extent: extent start: in position).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: input position\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         setStartAndEnd = ( |
            | 
            extent: extent startAndEnd: in position).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         source = ( |
            | in sourceAt: extent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | extent start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            nil = in  ifTrue: ['no inputStream']
                       False: [source]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: private'
        
         testResultString: depth = ( |
             r <- ''.
            | 
            preComments isEmpty ifFalse: [
              r: '( '.
              preComments do: [|:n| r: r, n source, ' '].
              r: r, ') '.
            ].
            r: r, ' ', basicTestResultString.
            postComments isEmpty ifFalse: [
              r: r, ' ( '.
              postComments do: [|:n| r: r, n source, ' '].
              r: r, ') '.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isCloseParen = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isColon = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isComma = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isDot = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fComment: Am I the last token in the input?\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isLast = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isLiteral = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isNumber = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isOpenParen = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTokens InitialContents: FollowSlot\x7fVisibility: public'
        
         isSemicolon = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 



 '-- Side effects'

 globals modules parseKitTokens postFileIn
