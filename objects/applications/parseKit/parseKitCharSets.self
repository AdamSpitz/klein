 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot'
        
         parseKitCharSets = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitCharSets.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'parseKitCharSets2
parseKitCharSets3
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         characterSets = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( |
             {} = 'Comment: I am a name space of sets of characters.\x7fModuleInfo: Creator: globals parseKit characterSets.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit characterSets abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         + aCharSetOrString = ( |
            | 
            union combine: self And: aCharSetOrString asCharacterSet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         - aCharSetOrString = ( |
            | 
            difference combine: self And: aCharSetOrString asCharacterSet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         => aCharSetOrString = ( |
            | 
            mustBeSingleton.
            aCharSetOrString asCharacterSet mustBeSingleton.
            range from: self first To: aCharSetOrString first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         asCharacterSet = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot'
        
         blockStructure* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         collectionName = ( |
            | 
            prototype asMirror safeName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: must be overridden\x7fCategory: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         concisePrintString = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: must be overridden\x7fCategory: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: must be overridden\x7fCategory: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: must be overridden\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         for: aString = ( |
            | 
            aString isEmpty   ifTrue: [^  empty].
            aString size = 1  ifTrue: [^ singleton copy setCharacter: aString].
            extension copy setCharacters: aString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: must be overridden\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         includes: k = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmutableForFilingOut = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         maxCharacter = 255.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         mustBeSingleton = ( |
            | 
            size = 1 ifFalse: [error: printString, ' must be a singleton'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'collection' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         printString = ( |
            | concisePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: must be overridden\x7fCategory: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         storeStringIfFail: fb = ( |
            | 
            concisePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractDyad = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit characterSets abstractDyad.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         combine: a And: b = ( |
            | ( ( copy set1: a ) set2: b ) init).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         concisePrintString = ( |
            | 
            '(', set1 concisePrintString, ')',
            ' ', infixConstructor, ' ',
            '(', set2 concisePrintString, ')').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            clone).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            error: 'does not apply').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         includes: k = ( |
            | 
            inclusionCache at: k asByte).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         difference = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( |
             {} = 'Comment: A character set defined by the union of two sets.\x7fModuleInfo: Creator: globals parseKit characterSets difference.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | 
            doCache do: [|:c| b value: c With: c]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         doCache <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         inclusionCache <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         infixConstructor = '-'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot'
        
         init = ( |
            | 
            inclusionCache: vector copySize: maxCharacter FillingWith: false.
            set1 do: [|:c| inclusionCache at: c asByte Put: true].
            set2 do: [|:c| (includes: c) ifTrue: [inclusionCache at: c asByte Put: false]
                                          False: [error: 'character: ', c, ' is in ', set1 printString,
                                                         ' but not in ', set2 printString]].
            doCache: (set1 asSet removeAll: set2) asString.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            difference).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         empty = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( |
             {} = 'Comment: The empty character set.
Override me for other kinds of character sets
and reimplement all my methods.\x7fModuleInfo: Creator: globals parseKit characterSets empty.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: private'
        
         set1 <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'difference' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: private'
        
         set2 <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         concisePrintString = ( |
            | collectionName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         includes: k = ( |
            | false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            empty).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         extension = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( |
             {} = 'Comment: A character set defined by its extension;
whatever characters you specify in a string.\x7fModuleInfo: Creator: globals parseKit characterSets extension.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         concisePrintString = ( |
            | 
            myCharacters storeString, ' asCharacterSet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            clone).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            error: 'does not apply').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | 
            myCharacters do: [|:c| b value: c With: c]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         includes: k = ( |
            | 
            inclusionCache at: k asByte).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         inclusionCache <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         myCharacters <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            extension).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'extension' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         setCharacters: s = ( |
            | 
            myCharacters: s.
            inclusionCache: vector copySize: maxCharacter succ FillingWith: false.
            s do: [|:c| inclusionCache at: c asByte Put: true].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         range = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( |
             {} = 'Comment: A character set defined by a closed interval
of characters.\x7fModuleInfo: Creator: globals parseKit characterSets range.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         concisePrintString = ( |
            | 
            firstChar storeString, ' asCharacterSet  =>  ',
            lastChar storeString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            clone).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            error: 'does not apply').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | 
            firstChar asByte to: lastChar asByte Do: [|:i. c| 
              c: i asCharacter.
              b value: c With: c]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (\'a\')\x7fVisibility: private'
        
         firstChar <- 'a'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         from: f To: l = ( |
            | ( copy firstChar: f) lastChar: l).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         includes: k = ( |
            | 
            (firstChar asByte <= k asByte)
            &&
            [k asByte <= lastChar asByte]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (\'z\')\x7fVisibility: private'
        
         lastChar <- 'z'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'range' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            range).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         singleton = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( |
             {} = 'Comment: A character set with one element.\x7fModuleInfo: Creator: globals parseKit characterSets singleton.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         concisePrintString = ( |
            | 
            myKey storeString, ' asCharacterSet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            clone).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            error: 'does not apply').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | 
            b value: myKey With: myKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         includes: k = ( |
            | 
            k = myKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (\'?\')\x7fVisibility: private'
        
         myKey <- '?'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            singleton).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'singleton' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         setCharacter: c = ( |
            | myKey: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: general\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         union = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( |
             {} = 'Comment: A character set defined by the union of two sets.\x7fModuleInfo: Creator: globals parseKit characterSets union.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         do: b = ( |
            | 
            set1 do: b.
            set2 do: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         inclusionCache <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         infixConstructor = '+'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot'
        
         init = ( |
            | 
            inclusionCache: vector copySize: maxCharacter FillingWith: false.
            do: [|:c| inclusionCache at: c asByte Put: true].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'abstractDyad' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            union).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: private'
        
         set1 <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets InitialContents: InitializeToExpression: (parseKit characterSets empty)\x7fVisibility: private'
        
         set2 <- bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> 'empty' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'string' -> () From: ( | {
         'Category: parseKit\x7fCategory: character sets\x7fModuleInfo: Module: parseKitCharSets InitialContents: FollowSlot\x7fVisibility: public'
        
         asCharacterSet = ( |
            | 
            parseKit characterSets extension for: self).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'parseKitCharSets2' From: 'applications/parseKit'
 bootstrap read: 'parseKitCharSets3' From: 'applications/parseKit'



 '-- Side effects'

 globals modules parseKitCharSets postFileIn
