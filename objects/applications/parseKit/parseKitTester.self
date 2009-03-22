 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot'
        
         parseKitTester = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitTester.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitTester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         tester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit tester.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit tester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: bebugging\x7fCategory: mutating the source\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         addTokenTo: src = ( |
             i.
             tokToAdd.
             toks.
            | 
            toks: sourceToNoncommentParser
              copyParseString: src
                       IfFail: [|:e| error: e].
            tokToAdd: toks subnodeAt: random integer: toks subnodeCount.
            i: (toks subnodeAt: random integer: toks subnodeCount) start offset.
            (src copySize: i),
            '  /*-->*/ ',
            tokToAdd source,
            ' /*<--*/  ',
            (src copyFrom: i)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: bebugging\x7fCategory: top-level operations\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         addTokenToFile: testName NTimes: n = ( |
            | 
            alterTestFile: testName
               NTimes: n
                   By: [|:src| addTokenTo: src]
               Suffix: '_a').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: bebugging\x7fCategory: mutating files\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         alterTestFile: testName NTimes: n By: alterBlock Suffix: suf = ( |
            | 
            1 to: n Do: [|:i| alterTestFile: testName Suffix: suf, '_', i printString By: alterBlock].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: bebugging\x7fCategory: mutating files\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         alterTestFile: testName Suffix: suf By: alterBlock = ( |
             i.
             ns.
             src.
             tokToRm.
             toks.
            | 
            src: getSourceFor: testName.
            ns: alterBlock value: src.
            suf isEmpty ifTrue: [error: 'I doubt that you really want to change ', fileName].
            setSourceFor: testName, suf To: ns.
            testify: testName, suf).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: relics from old scheme\x7fCategory: lexical manipulation\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         appendFinalComment: cmt To: src = ( |
            | 
            src, '\n', (commentify: cmt)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fCategory: comments\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         commentPrefix = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fCategory: comments\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         commentSuffix = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fCategory: comments\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         commentify: cmtSrc = ( |
            | 
            commentPrefix, cmtSrc, commentSuffix).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: file operations\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         contentsOf: fileName = ( |
            | 
            fileName asFileContents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: relics from old scheme\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot'
        
         convert: testName = ( |
            | 
            setSourceFor: testName
            To:
             (removeFinalNewlinesFrom:
                removeFinalCommentFrom:
                  removeFinalNewlinesFrom:
                    getSourceFor: testName) , '\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: relics from old scheme\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot'
        
         convertAll = ( |
            | 
            tests do: [|:t|
            convert: t]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: relics from old scheme\x7fCategory: lexical manipulation\x7fComment: The final comment stuff used to be
used on an older version of this stuff.\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         finalCommentOf: src IfAbsent: ab = ( |
             r.
             tokens.
            | 
            tokens: lex: src IfFail: [|:e. s|
              "nonlexing tests must be one line"
              s: src.
              [s isEmpty || ['\n\r' includes: s first]]
               whileFalse: [s: s copyWithoutFirst ].
              s isEmpty ifFalse: [s: s copyWithoutFirst].
              lex: s IfFail: [|:e|    
                error: 'could not lex source: ', e printString
              ].
            ].
            tokens removeLast isLast ifFalse: [
              error: 'last token should be EOF token'
            ].
            tokens isEmpty not && [tokens last isComment]
              ifTrue: [tokens last source]
               False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         generateTestResultFor: source = ( |
             parseTree.
            | 
            parseTree: parse: source IfFail: [|:e|
              ^ e testResultString
            ].
            parseTree testResultString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: getting & setting test input and results\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         getSourceFor: testName = ( |
            | 
            contentsOf: sourceFileNameFor: testName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: getting & setting test input and results\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         getTestResultFor: testName = ( |
            | 
            contentsOf: resultFileNameFor: testName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         lex: source IfFail: failBlock = ( |
             lex.
            | 
            lex: lexer copyForInputStream: 
                   inputStream copyForString: source.
            (lex scanAllTokensIfFail: [|:e| ^ failBlock value: e]) subnodes copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         nameSeparator = '/'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parse: source IfFail: failBlock = ( |
            | 
            theParser copyParseSource: source IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fComment: points to a name space that defines lexer and inputStream\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         prototypes* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> 'prototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit tester parent prototypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: relics from old scheme\x7fCategory: lexical manipulation\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         removeFinalCommentFrom: src = ( |
             r.
            | 
            r: src copySize: src size -  (finalCommentOf: src IfAbsent: [^src]) size.
            "appendFinalComment will add a newline"
            r isEmpty not && [r last = '\n'] ifTrue: [r: r copyWithoutLast].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: relics from old scheme\x7fCategory: lexical manipulation\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         removeFinalNewlinesFrom: src = ( |
             r.
            | 
            r: src asList.
            [ r isEmpty not && ['\n\r' includes: r last] ] whileTrue: [r removeLast].
            r asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: bebugging\x7fCategory: mutating the source\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         removeTokenFrom: src = ( |
             i.
             ns.
             tokToRm.
             toks.
            | 
            toks: sourceToNoncommentParser
              copyParseString: src
                       IfFail: [|:e| error: e].
            i: random integer: toks subnodeCount.
            tokToRm: toks subnodeAt: i.
            ns: src copySize: tokToRm start offset.
            ns: ns, '/*-->> ', tokToRm source, ' <<--*/'.
            ns: ns, (src copyFrom: tokToRm end offset succ).
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: bebugging\x7fCategory: top-level operations\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         removeTokenFromFile: testName NTimes: n = ( |
            | 
            alterTestFile: fileName
               NTimes: n
                   By: [|:src| removeTokenFrom: src]
               Suffix: '_x').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: file names\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         resultFileNameFor: testName = ( |
            | 
            testDirectory, nameSeparator, 'tests',
            nameSeparator, 'results', nameSeparator, testName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test lists\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         selectTestsAtRandom: howMany = ( |
            | 
            tests asSet selectAtRandom: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: file operations\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         setContentsOf: fileName To: newContents = ( |
            | 
            fileName setFileContentsTo: newContents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: getting & setting test input and results\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         setSourceFor: testName To: newSource = ( |
            | 
            setContentsOf: (sourceFileNameFor: testName)
              To: newSource).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: getting & setting test input and results\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         setTestResultOf: testName To: newSource = ( |
            | 
            setContentsOf: (resultFileNameFor: testName)
              To: newSource).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: file names\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceFileNameFor: testName = ( |
            | 
            testDirectory, nameSeparator, 'tests',
            nameSeparator, testName, testFileSuffix).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: top-level testing\x7fCategory: individuals\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         test: testName = ( |
             is.
             is1.
             shouldBe.
             shouldBe1.
             src.
            | 
            src: getSourceFor: testName.
            is: generateTestResultFor: src.
            shouldBe: getTestResultFor: testName.
            "check-ins introduce newlines, mac has crs"
                  is1:       is fixLineBreaksForPrinting.
            shouldBe1: shouldBe fixLineBreaksForPrinting.

                  is1 last = '\n'  ifTrue: [      is1:       is1 copyWithoutLast].
            shouldBe1 last = '\n'  ifTrue: [shouldBe1: shouldBe1 copyWithoutLast].

            is1 = shouldBe1 ifFalse: [
              error: 'test of ', testName, ' failed. Should be:\n<<', shouldBe, '>>\n\nis:\n<<', is, '>>'
            ].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: top-level testing\x7fCategory: groups\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         testAll = ( |
            | 
            tests do: [|:t| t printLine. test: t].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectory = '.'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         testFileSuffix = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: top-level testing\x7fCategory: individuals\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         testify: testName = ( |
             r.
             src.
            | 
            src: getSourceFor: testName.
            r: generateTestResultFor: src.
            setTestResultOf: testName To: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: top-level testing\x7fCategory: groups\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         testifyAll = ( |
            | 
            tests do: [|:t| testify: t].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: top-level testing\x7fCategory: groups\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: public'
        
         testifyFirst = ( |
            | testify: tests first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: building test lists\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         tests: aNumber Named: nameStem = ( |
            | 
            (vector copySize: aNumber)
            mapBy: [|:e. :i| nameStem, i succ asString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         theParser = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: override these\x7fCategory: comments\x7fModuleInfo: Module: parseKitTester InitialContents: FollowSlot\x7fVisibility: private'
        
         uncommentify: wholeComment = ( |
            | 
            (commentPrefix isPrefixOf: wholeComment) ifFalse: [
              error: 'wrong comment prefix'
            ].
            (commentSuffix isSuffixOf: wholeComment) ifFalse: [
              error: 'wrong comment suffix'
            ].
            (wholeComment 
              copySize: wholeComment size - commentSuffix size)
              copyFrom: commentPrefix size).
        } | ) 



 '-- Side effects'

 globals modules parseKitTester postFileIn
