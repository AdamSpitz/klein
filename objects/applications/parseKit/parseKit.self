 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot'
        
         parseKit = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKit.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKit' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'parseKitCharSets
parseKitInput
parseKitErr
parseNode
parseKitMinis
parseKitNodes
parseKitSamples
parseKit1
parseKitUI
parseKitTester
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fComment: a parser framework for klein\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         parseKit = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         lexer = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( |
             {} = 'Comment: To make your lexer,
create a copy-down child of
this object and replace the contents of the spaces 
and tokens slots. -- dmu 8/99\x7fModuleInfo: Creator: globals parseKit lexer.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: input\x7fModuleInfo: Module: parseKit InitialContents: InitializeToExpression: (parseKit inputStream)\x7fVisibility: private'
        
         myInputStream <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: results (tokens & errors)\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         myNext <- bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: results (tokens & errors)\x7fModuleInfo: Module: parseKit InitialContents: InitializeToExpression: (parseKit noError)\x7fVisibility: private'
        
         myNextError <- bootstrap stub -> 'globals' -> 'parseKit' -> 'noError' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: results (tokens & errors)\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         myPrevious <- bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         checkSpacesDisjointFromInitialCharacters = ( |
            | 
            spaceCharacters do: [|:char|
              (tokenMap includesKey: char) ifTrue: [
                error: 'Character ', char printString, 
                       ' is both a space character and an initial character for token: ',
                       (tokenMap at: char) printString.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForInputStream: is = ( |
            | 
            initPrototype clone myInputStream: is).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         forceInit = ( |
            | 
            spaceCharacters: nameSpace characterSets empty.
            tokenMap: dictionary copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         in = ( |
            | myInputStream).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fComment: placeholder for overriding\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         initPrototype = ( |
            | 
            spaceCharacters isEmpty ifTrue: [ initSpaceCharacters ].
            tokenMap        isEmpty ifTrue: [ initTokenMap ].
            checkSpacesDisjointFromInitialCharacters.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fCategory: initialization helpers\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         initSpaceCharacters = ( |
            | 
            spaceCharacters: spaces asCharacterSet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fCategory: initialization helpers\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         initTokenMap = ( |
             tm.
            | 
            tm: dictionary copyRemoveAll.
            tokenPrototypes do: [|:aToken|
              aToken initialCharacterSet do: [|:c|
                tm 
                  if: c
                  IsPresentDo: [|:otherToken|
                  ^ error:  tokens: otherToken And: aToken StartWith: c
                  ]
                  IfAbsentPut: [ aToken ]  AndDo: [].
              ].
            ].
            tokenMap: tm.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: tokens\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         next = ( |
            | nextIfFail: [|:e| error: e printString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: tokens\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         nextIfFail: fb = ( |
            | 
            myPrevious: peekIfFail: [|:e| ^ fb value: e].
            myNext: token.
            myPrevious).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: tokens\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         peek = ( |
            | peekIfFail: [|:e| error: e printString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: tokens\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         peekIfFail: fb = ( |
            | 
            myNextError = noError ifFalse: [^ fb value: myNextError].
            myNext      = token   ifFalse: [^ myNext].
            myNext: scanNextTokenIfFail: [|:e|
              myNextError: e.
              token
            ].
            peekIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: tokens\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         previous = ( |
            | myPrevious).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         scanAllTokensIfFail: fb = ( |
             r.
            | 
            r: nameSpace parseNodes node copyRemoveAll.
            [ |t|
              t: scanNextTokenIfFail: [|:e| ^ fb value: e].
              r addSubnode: t.
              t isLast
            ] whileFalse.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         scanNextPossiblyMeaninglessTokenIfFail: fb = ( |
             c.
             t.
             tproto.
            | 
            [ 
              c: in peekIfEOF: [
                ^ (tokens eof copyForLexer: self) scanIfFail: fb.
              ].
              spaceCharacters includes: c
            ] whileTrue: [ in advance ].

            tproto:
              tokenMap at: c 
                 IfAbsent: [^ fb value: syntaxErrorForNoPossibleToken ].
            (tproto copyForLexer: self) scanIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         scanNextTokenIfFail: fb = ( |
             t.
            | 
            [
              t: scanNextPossiblyMeaninglessTokenIfFail: [|:e|
                ^ fb value: e
              ]
            ] untilTrue: [t isMeaningful].
            t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         shorterNaming* = bootstrap stub -> 'globals' -> 'parseKit' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: scanning\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         syntaxErrorForNoPossibleToken = ( |
             e.
            | 
            e: inputExtent start: in position.
            in advance.
            e: e end: in position.
            syntaxError reason: 'No possible token could start with'
                         Input: in
                        Extent: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         testString: input = ( |
             l.
             tokens.
            | 

            l: copyForInputStream: parseKit inputStream copyForString: input.
            tokens: l scanAllTokensIfFail: [|:err| error: err printString ].
            tokens do: [|:tok|
              (tok printString, ' at: ', tok extent statePrintString) printLine
            ].
            tokens).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         testToken: testString ShouldBe: tokenName = ( |
            | 
            testToken: testString ShouldBe: tokenName Value: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         testToken: testString ShouldBe: tokenName Value: desiredValue = ( |
             l.
             token.
            | 
            l: copyForInputStream: inputStream copyForString: testString.
            token: l scanNextPossiblyMeaninglessTokenIfFail: [|:e|
              error: '`', testString, '\' should succeed, but failed: ', e printString
            ].
            token asMirror prototypeIfPresent: [|:pm|
              pm = (tokenName sendTo: tokens) asMirror
               ifFalse: [error: '`', testString, '\' should lex to a ', tokenName,
                                ' but resulted in a ', pm name].
            ] IfAbsent: [|:e| error: '`', testString, '\' should lex to a ', tokenName,
                                     ' but token had no prototype: ', e].

            (desiredValue = nil) || [token value = desiredValue]  ifFalse: [
              error: '`', testString, '\' should have value ',
                     desiredValue printString, ' but lexed to ', token value printString
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         testToken: testString ShouldFail: msg Selecting: selection = ( |
             l.
             token.
            | 
            l: copyForInputStream: inputStream copyForString: testString.
            token: l scanNextPossiblyMeaninglessTokenIfFail: [|:e|
              e reason = msg ifFalse: [
                error: '`', testString, '\' should have failed because: ',
                       msg, ' but failed because: ', e reason
              ].
              e source = selection ifFalse: [
                error: '`', testString, '\' should have failed selecting: ',
                       selection, ' but failed selecting: ', e source
              ].
              ^ self
            ].
            error: '`', testString, '\' should have failed, but lexed to: ',
                    token printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         testTokenFailures = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         testTokenSuccesses = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         testTokens = ( |
            | 
            testTokenSuccesses.
            testTokenFailures).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fCategory: initialization helpers\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         tokenPrototypes = ( |
            | 
            ((reflect: tokens) 
              asList copyFilteredBy: [|:s| s isParent not ] 
            )        copyMappedBy:   [|:s| s contents reflectee]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'parent' -> () From: ( | {
         'Category: creating\x7fCategory: initialization helpers\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         tokens: firstTok And: secondTok StartWith: aChar = ( |
            | 
            'Two tokens (',
            firstTok printString,
            ' and ',
            secondTok printString,
            ') both start with the same character: ',
            aChar storeString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: guides\x7fModuleInfo: Module: parseKit InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: private'
        
         spaceCharacters <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: change these to lex your language\x7fComment: a string with all the whitespace characters
for your language\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         spaces = ' '.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: guides\x7fComment: maps first char of a token to
the prototype for that token\x7fModuleInfo: Module: parseKit InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         tokenMap <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: change these to lex your language\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         tokenTraits = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokenTraits' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokenTraits.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokenTraits' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         valueTokens = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokenTraits' -> 'valueTokens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokenTraits valueTokens.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokenTraits' -> 'valueTokens' -> () From: ( | {
         'ModuleInfo: Module: parseKit InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> 'token' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokenTraits' -> 'valueTokens' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            value printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> () From: ( | {
         'Category: change these to lex your language\x7fComment: An object containing constant slots;
one holding each token to be selected
based in initial characters.\x7fModuleInfo: Module: parseKit InitialContents: FollowSlot\x7fVisibility: private'
        
         tokens = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'lexer' -> 'tokens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit lexer tokens.
'.
            | ) .
        } | ) 



 '-- Sub parts'

 bootstrap read: 'parseKitCharSets' From: 'applications/parseKit'
 bootstrap read: 'parseKitInput' From: 'applications/parseKit'
 bootstrap read: 'parseKitErr' From: 'applications/parseKit'
 bootstrap read: 'parseNode' From: 'applications/parseKit'
 bootstrap read: 'parseKitMinis' From: 'applications/parseKit'
 bootstrap read: 'parseKitNodes' From: 'applications/parseKit'
 bootstrap read: 'parseKitSamples' From: 'applications/parseKit'
 bootstrap read: 'parseKit1' From: 'applications/parseKit'
 bootstrap read: 'parseKitUI' From: 'applications/parseKit'
 bootstrap read: 'parseKitTester' From: 'applications/parseKit'



 '-- Side effects'

 globals modules parseKit postFileIn
