 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseKitMinis = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitMinis.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitMinis' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         miniParser = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit miniParser.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (nil)'
        
         errorNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myFailBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (nil)'
        
         nodesToParse.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit miniParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParse: nodesToParse AtOrAfter: errorNode IfFail: failBlock = ( |
            | 
            (((
              copy
                nodesToParse: nodesToParse)
                errorNode:    errorNode)
                myFailBlock:  failBlock)
                  parse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParseNonEmpty: nodesToParse IfFail: failBlock = ( |
            | 
            copyParse: nodesToParse
            AtOrAfter: nodesToParse first
            IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: lexing and parsing\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParseSource: s = ( |
            | 
            copyParseSource: s IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: lexing and parsing\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParseSource: s IfFail: failBlock = ( |
             treeOfNoncomments.
             treeOfParsedBrackets.
            | 
            treeOfNoncomments: lexString: s ToNonCommentTokensIfFail: [|:e| ^ failBlock value: e].
            treeOfParsedBrackets: parseBracketsFrom: treeOfNoncomments IfFail: [|:e| ^ failBlock value: e].
            copyParseNonEmpty: treeOfParsedBrackets subnodes copy IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         failAll: nodes Because: m = ( |
            | 
            failFrom: nodes first To: nodes last Because: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         failBecause: m = ( |
            | 
            failNode: errorNode Because: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         failFrom: startNode To: endNode Because: m = ( |
            | 
            failInput: startNode in
                   At: ((startNode start) to: (endNode end))
              Because: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         failInput: in At: extent Because: m = ( |
            | 
            myFailBlock value:
              parseKit syntaxError
                reason: m
                 Input: in 
                Extent: extent.
            error: 'should not reach here').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: error handling\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         failNode: n Because: m = ( |
            | 
            failInput: n in 
                   At: n extent
              Because: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: lexing and parsing\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         lexString: s ToNonCommentTokensIfFail: fb = ( |
            | 
            sourceToNoncommentParser
              copyParseString: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: lexing and parsing\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseBracketsFrom: aNode IfFail: fb = ( |
            | 
            aNode subnodeCount = 0 ifTrue: [^aNode].
            bracketParser copyParseNonEmpty: aNode subnodes copy IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: lexing and parsing\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInnerBracketsFrom: aNode IfFail: fb = ( |
             closeBracket.
             nodes.
             openBracket.
             r.
             t.
            | 
            "This parser wants the CONTENTS of the top-level paren-list,
             so hide opening and closing brackets from the bracket parser."

            nodes: aNode subnodes copy.
            openBracket: nodes removeFirstIfAbsent: [^ aNode].
            closeBracket: nodes removeLastIfAbsent: [^ aNode].
            closeBracket isLast ifTrue: [
              closeBracket: nodes removeLastIfAbsent: [^ aNode].
            ].
            t: bracketParser copyParseNonEmpty: nodes IfFail: [|:e| ^ fb value: e].

            r: (((
                  node copyRemoveAll 
                  addSubnode: openBracket)
                  addAllSubnodes: t subnodes)
                  addAllSubnodes: nodes)
                  addSubnode: closeBracket.

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         shortHand* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (nil)'
        
         result.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         bracketParser = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'result' From:
             globals parseKit miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit bracketParser.

CopyDowns:
globals parseKit miniParser. copy 
SlotsToOmit: parent result.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit bracketParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         closers = ')}]' asCharacterSet.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         openers = '({[' asCharacterSet.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
            | 
            resultStack: list copyRemoveAll addLast: parseKit parseNodes node copyRemoveAll.
            [nodesToParse isEmpty] whileFalse: [parseNode: nodesToParse removeFirst].
            resultStack size = 1  ifTrue: [^ resultStack last].

            failFrom: resultStack last
                  To: errorNode
             Because: 'unmatched open bracket').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseClose: n = ( |
             bracketNode.
             t.
            | 
            t: resultStack last.
            case if:   ( t isBracketedList not )  
                 Then: [ failFrom: t
                               To: n
                          Because: 'closing bracket without an open' ]
                 If:   [ t closingSource != n source ]
                 Then: [ failFrom: t
                               To: n
                          Because: 'Opening `', 
                                   t firstSubnode source,
                                   '\' does not match closing `', 
                                   n source, '\' in'].
            parseNeutral: n.
            bracketNode: resultStack last.
            resultStack removeLast.
            parseNeutral: bracketNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseNeutral: n = ( |
            | 
            resultStack last addSubnode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseNode: n = ( |
             s.
            | 
            errorNode: n.
            n isToken ifFalse: [^parseNeutral: n].
            s: n source.
            s size = 1  ifFalse: [^parseNeutral: n].
            (closers includes: s) ifTrue: [^parseClose: n].
            (openers includes: s) ifTrue: [^parseOpen:  n].
            parseNeutral: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseOpen: n = ( |
             o.
             s.
            | 
            s: n source.
            o: case if: ( s = '(' )  Then: parenList 
                    If: [ s = '[' ]  Then: squareList
                    If: [ s = '{' ]  Then: curlyList
                    Else: [ error: 'should be one of ([{' ].
            resultStack addLast: o copy addSubnode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'bracketParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (nil)'
        
         resultStack.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         commentParser = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit commentParser.

CopyDowns:
globals parseKit miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         comments <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit commentParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy comments: comments copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         finish = ( |
             nn.
            | 
            "end of nodes; may have unattached comments"
            comments isEmpty ifTrue: [^ self]. "nope, AOK"
            nil = prevNoncomment  ifTrue: [
              "no node to attach comments to"
              comments do: [|:n| result addSubnode: n].
              ^ self
            ].
            "attach comments to last node"
            nn: prevNoncomment copy.
            comments do: [|:n| nn addPostComment: n].
            result removeLastSubnode.
            result addSubnode: nn.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
            | 
            result: parseKit parseNodes node copyRemoveAll.
            [nodesToParse isEmpty] whileFalse: [parseNode: nodesToParse removeFirst].
            finish.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseComment: n = ( |
            | 
            case if: (nil = prevNoncomment) 
                 Then: [comments addLast: n] "no prev noncomment"
                 If: [prevNoncomment end line != n start line]
                 Then: [comments addLast: n] "not on same line"
                 Else: [prevNoncomment addPostComment: n].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseNode: n = ( |
            | 
            n isToken &&  [n isComment]
              ifTrue: [parseComment: n]
               False: [parseNoncomment: n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parseNoncomment: n = ( |
             nn.
            | 
            comments isEmpty
              ifTrue: [ nn: n ]
               False: [ nn: n copy preComments: comments.
                        comments: comments copyRemoveAll.
                      ].
            prevNoncomment: nn.
            result addSubnode: nn.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'commentParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (nil)'
        
         prevNoncomment.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         separatorListParser = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit separatorListParser.

CopyDowns:
globals parseKit miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit separatorListParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParse: nodes AtOrAfter: errorNode Separator: sepString IfFail: failBlock = ( |
            | 
            ((((
              copy
                nodesToParse: nodesToParse)
                errorNode:    errorNode)
                separator:    sepString)
                myFailBlock:  failBlock)
                  parse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParseNonEmpty: nodesToParse Separator: sepString IfFail: failBlock = ( |
            | 
            copyParse: nodesToParse
            AtOrAfter: nodesToParse firstSubnode
            Separator: sepString
            IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            result: parseKit parseNodes separatorList copy.
            [
              result addNonseparator: parseKit parseNodes node copy.
              nodesToParse isEmpty not
              && [nodesToParse first source != separator]
                whileTrue: [
                  result lastSubnode addSubnode: nodesToParse removeFirst.
              ].
              nodesToParse isEmpty ifTrue: [^ result].
              result addSeparator: nodesToParse removeFirst
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'separatorListParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (\'\')'
        
         separator <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceToNoncommentParser = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit sourceToNoncommentParser.

CopyDowns:
globals parseKit miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit sourceToNoncommentParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParseString: aString IfFail: failBlock = ( |
            | 
            ((copy 
            sourceString: aString)
            myFailBlock: [|:e| ^ failBlock value: e])
            parse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         hideComments = ( |
            | 
            result: parseKit commentParser
                      copyParseNonEmpty: nodesToParse "must incl EOF token"
                                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         inputStreamForSource: str = ( |
            | 
            inputStream copyForString: sourceString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         lexSource = ( |
             p.
            | 
            p: lexer copyForInputStream: inputStreamForSource: sourceString.
            nodesToParse: (p scanAllTokensIfFail: myFailBlock) subnodes copy.
            errorNode: nodesToParse first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            lexSource.
            hideComments.

            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (\'\')'
        
         sourceString <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (parseKit parseNodes node)'
        
         treeOfTokens <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         terminatorListParser = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit terminatorListParser.

CopyDowns:
globals parseKit miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit terminatorListParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParse: nodesToParse AtOrAfter: errorNode Terminator: termString IfFail: fb = ( |
            | 
            ((((
              copy
                nodesToParse: nodesToParse)
                errorNode:    errorNode)
                myFailBlock:  failBlock)
                terminator: termString)
                  parse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         copyParseNonEmpty: nodesToParse Terminator: termString IfFail: failBlock = ( |
            | 
            copyParse: nodesToParse
            AtOrAfter: nodesToParse firstSubnode
            Terminator: termString
            IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            result: parseKit parseNodes terminatorList copy.
            [
              result addNonterminator: parseKit parseNodes node copy.
              nodesToParse isEmpty not
              && [nodesToParse first source != terminator]
                whileTrue: [
                  result lastSubnode addSubnode: nodesToParse removeFirst.
              ].
              nodesToParse isEmpty ifTrue: [
                failNode: result lastSubnode extent
                 Because: 'Missing `', terminator, '\' after'
              ].
              result addTerminator: nodesToParse removeFirst.
              nodesToParse isEmpty ifTrue: [ ^ result].
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'terminatorListParser' -> () From: ( | {
         'ModuleInfo: Module: parseKitMinis InitialContents: InitializeToExpression: (\'\')'
        
         terminator <- ''.
        } | ) 



 '-- Side effects'

 globals modules parseKitMinis postFileIn
