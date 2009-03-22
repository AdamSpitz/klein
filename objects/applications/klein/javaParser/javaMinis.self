 '$Revision: 30.10 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         miniParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser miniParser.

CopyDowns:
globals parseKit miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser miniParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         arrayify: baseType = ( |
             listOSquares.
            | 
            listOSquares:
              emptySquaresParser 
                copyParse: nodesToParse
                AtOrAfter: baseType
                   IfFail: myFailBlock.
            listOSquares isEmpty
              ifTrue: [baseType]
               False: [parseNodes arrayType
                         copyBaseType: baseType Squares: listOSquares]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         hasNodeBeforeTerminator = ( |
            | 
            "1 for ; or {}, 1 for token"
            nodesToParse size >= 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         ifFirstNodeIs: testBlock ParseWith: parserProto IfFound: fb = ( |
            | 
                  nodesToParse isEmpty not
            &&  [ testBlock value: nodesToParse first ]
              ifTrue: [ fb value: parserProto copyParseNonEmpty: nodesToParse
                                                         IfFail: [|:e| ^ myFailBlock value: e] ]
               False: [ self ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         mustNotBeEmpty = ( |
            | 
            nodesToParse isEmpty not  ifTrue: [^ self].
            failBecause: 'Statement had nothing to parse after').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         mustNotHaveModifiers: what = ( |
            | 
            modifiers = nil  ifFalse: [
              failNode: modifiers
               Because: what capitalize, ' should not have modifiers'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         nameSpace* = bootstrap stub -> 'globals' -> 'javaParser' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseArgumentList = ( |
            | 
            "first node to parse is a paren list"
            argumentListParser copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                                          IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseBasicReferenceType = ( |
            | 
            nodesToParse first isJavaIdentifier ifFalse: [
              failNode: nodesToParse first
               Because: 'Expected a type such as int or class-name, but found'
            ].
            parseNodes classOrInterfaceType copyName:
              nameParser copyParseNonEmpty: nodesToParse
                                     IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseBasicTypeIfFound: foundB = ( |
            | 
            nodesToParse first isJavaTypeKeyword ifFalse: [^ self].
            foundB value: 
              parseNodes basicType copyKeyword: nodesToParse removeFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseBlock = ( |
             b.
            | 
            nodesToParse isEmpty  ifTrue: [
              failBecause: 'Expected a block in curly brackets after'
            ].
            b: nodesToParse removeFirst.
            b isCurlyList ifFalse: [
              failNode: b
               Because: 'Expected a block in curly brackets instead of'
            ].
            blockParser copyParseNonEmpty: b subnodes copy
                                   IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseDimensionExpression: ns = ( |
             e.
             lft.
             r.
            | 
            lft: ns removeFirst.
              r: ns removeLast.

            ns isEmpty ifTrue: [
              ^ parseNodes dimensionExpression copyLeft: lft Right: r
            ].
            errorNode: lft.
            e: parseExpression: ns.
            parseNodes dimensionExpression
                copyLeft: lft
              Expression: e
                   Right: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseDimensionExpressions = ( |
             dimExprs.
             hadEmpty <- bootstrap stub -> 'globals' -> 'false' -> ().
            | 

            nodesToParse isEmpty not
            && [ nodesToParse first isSquareList ]
             ifFalse: [ failBecause: 'Expected `[\' after'].

            dimExprs: list copyRemoveAll.
            [| sql. ns. r. lft. dimExpr |
              sql: nodesToParse removeFirst.
              dimExpr: parseDimensionExpression: sql subnodes copy.
              hadEmpty && [ dimExpr hasExpression ]
               ifTrue: [
                failNode: dimExpr
                 Because: 'Cannot have expression in brackets after empty brackets'
              ].
              hadEmpty: dimExpr hasExpression not.
              dimExprs addLast: dimExpr.
            ] untilFalse: [ nodesToParse isEmpty not && [nodesToParse first isSquareList]].
            dimExprs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpression1 = ( |
            | 
            expression1Parser copyParse: nodesToParse
                              AtOrAfter: errorNode
                                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpression2 = ( |
            | 
            expression2Parser copyParse: nodesToParse
                              AtOrAfter: errorNode
                                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpression3 = ( |
            | 
            expression3Parser copyParse: nodesToParse
                              AtOrAfter: errorNode
                                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpression: nodes = ( |
             e.
            | 
            e: expressionParser copyParse: nodes
                                AtOrAfter: errorNode
                                   IfFail: [|:e| ^ myFailBlock value: e].
            nodes isEmpty ifFalse: [
              failAll: nodes
              Because: 'Extra junk found at end of expresion'
            ].
            e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpression: nodes UpTo: chars = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            [nodes isEmpty
            || [chars includes: nodes first source]]
              whileFalse: [ns addLast: nodes removeFirst].
            parseExpression: ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpressionParenList = ( |
             e.
             exprNodes.
             pl.
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'expected ` ( expression ) \' after'
            ].
            pl: nodesToParse removeFirst.
            errorNode: pl.
            pl isParenList && [pl subnodeCount >= 3]  ifFalse: [
              failBecause: 'Expected ` ( expression ) \' instead of'
            ].
            exprNodes: pl subnodes copy.
            errorNode: exprNodes removeFirst.
            exprNodes removeLast.

            e: parseExpression: exprNodes.
            ((parseNodes parenList copyRemoveAll
                addSubnode: pl firstSubnode)
                addSubnode: e)
                addSubnode: pl lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseKeyword: kw = ( |
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'Expected `', kw, '\' after'
            ].
            errorNode: nodesToParse removeFirst.
            (errorNode isJavaKeyword: kw)  ifFalse: [
              failBecause: 'Expected `', kw, '\' instead of'
            ].
            errorNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseModifiers = ( |
            | 
            modifiers: 
              modifiersParser
                copyParse: nodesToParse
                AtOrAfter: errorNode
                   IfFail: [|:e| ^ myFailBlock value: e].
            nil = modifiers ifFalse: [errorNode: modifiers].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parsePrimary = ( |
            | 
            primaryParser copyParse: nodesToParse
                          AtOrAfter: errorNode
                             IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseQualifiedIdentifier = ( |
            | 
            qualifiedIdentifierParser
              copyParse: nodesToParse
              AtOrAfter: errorNode
                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseResultType = ( |
            | 
            "nodesToParse must include at least the semicolon or block at end
             of statement"

            hasNodeBeforeTerminator   ifFalse: [
              failAll: nodesToParse Because: 'No result type found for'
            ].
            resultType: typeParser
                          copyParse: nodesToParse
                          AtOrAfter: errorNode
                             IfFail: [|:e| ^ myFailBlock value: e].
            errorNode: resultType.

            hasNodeBeforeTerminator  ifFalse: [
              failBecause: 'Missing attribute name after type'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseSemicolon = ( |
            | 
            nodesToParse isEmpty  ifTrue: [
              failBecause: 'Expected semicolon after'
            ].
            nodesToParse first isSemicolon  ifFalse: [
              failNode: nodesToParse first Because: 'Extra junk found in statement'
            ].
            nodesToParse removeFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSimpleName = ( |
             n.
            | 
            nodesToParse isEmpty not && [n: nodesToParse removeFirst. n isJavaIdentifier]
              ifFalse: [
                failNode: result classOrInterfaceToken
                 Because: 'Could not find ', result classOrInterface, ' name identifer after'
            ].
            result nameToken: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatementExpression = ( |
            | 
            statementExpressionParser
              copyParse: nodesToParse 
              AtOrAfter: errorNode
                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatementExpressionList = ( |
             exprs.
             nodes.
            | 
            exprs: list copyRemoveAll.
            nodes: list copyRemoveAll.

            [|:exit. e|
              e: parseStatementExpression. 
              exprs addLast: e.
              nodes addLast: e.
              errorNode: e.
              nodesToParse isEmpty not && [nodesToParse first isComma]
                ifFalse: exit.
              e: nodesToParse removeFirst.
              nodes addLast: e.
              errorNode: e.
            ] loopExit.

            parseNodes statementExpressionList copyExpressions: exprs Nodes: nodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseType = ( |
            | 
            parseType: nodesToParse IfFound: [|:r| ^ r].
            failBecause: 'Expected type after').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> () From: ( | {
         'Category: pieces of statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseType: nodes IfFound: fb = ( |
             ns.
             r.
            | 
            "look for type followed by an id"
            ns: nodes copy.
            r: typeParser copyParse: nodes
                          AtOrAfter: errorNode
                             IfFail: [  nodes removeAll addAll: ns.
                                      ^ self ].
            errorNode: r.
            fb value: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         absStatementOrDclParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser absStatementOrDclParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser absStatementOrDclParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
            | 
            parseInContext).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: methods\x7fCategory: method pieces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseBody = ( |
             unparsedBody.
            | 

            unparsedBody: nodesToParse removeFirst.

            case if:   [unparsedBody isCurlyList]
                 Then: [ blockParser copyParseNonEmpty: unparsedBody subnodes copy 
                                                IfFail: myFailBlock ]
                 If:   [unparsedBody isSemicolon]
                 Then: [unparsedBody ]
                 Else: [ failNode: unparsedBody 
                          Because: 'Method body is not semicolon or {...}' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: other statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseEmptyStatementIfFound: fb = ( |
            | 
                 nodesToParse isEmpty not
            && [ nodesToParse first isSemicolon ]
             ifFalse: [^ self].

            fb value: parseNodes emptyStatement 
                        copySemicolon: nodesToParse removeFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseFieldDcl = ( |
             name.
             sqs.
            | 
            nodesToParse isEmpty not
            && [nodesToParse first isJavaIdentifier]
             ifFalse: [
              failBecause: 'Expected identifier after'.
            ].
            name: nodesToParse removeFirst.
            sqs: emptySquaresParser
                   copyParse: nodesToParse
                   AtOrAfter: name
                      IfFail: myFailBlock.

                nodesToParse isEmpty not
            && [nodesToParse first isJavaOperator: '=']
             ifFalse: [
              parseNodes variableDeclarator
                           copyName: name Squares: sqs
            ]
            True: [  |eq. initializer|
              eq: nodesToParse removeFirst.
              initializer: variableInitializerParser
                copyParse: nodesToParse
                AtOrAfter: eq
                   IfFail: [|:e| ^ myFailBlock value: e].
              parseNodes variableDeclarator
                            copyName: name Squares: sqs 
                              Equals: eq Initializer: initializer
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseFieldDclsIfFound: fb = ( |
             declarators.
             semi.
            | 
            nodesToParse isEmpty ifTrue: [^self].
            nodesToParse first isJavaIdentifier ifFalse: [^ self].

            declarators: list copyRemoveAll.

            [|:exit. name. sqs. eq. initializer |
              declarators addLast: parseFieldDcl.

              nodesToParse isEmpty        ifTrue: [^ self].
              nodesToParse first isComma  ifFalse: exit.

              errorNode: nodesToParse removeFirst.
              declarators last addComma: errorNode.
            ] loopExit.

            semi: nodesToParse removeFirst.
            "might be paren list for method"
            semi isParenList ifTrue: [^ self].
            semi isSemicolon ifFalse: [
              failNode: semi
              Because: 'Expected `;\' in field declaration instead of'
            ].
            fb value: parseNodes varDclsStatement
              copyModifiers: modifiers
                       Type: resultType
                Declarators: declarators
                  Semicolon: semi).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: methods\x7fCategory: method pieces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseFormals = ( |
            | 
            formalParameterListParser
              copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                         IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseLocalVarDclStmtIfFound: fb = ( |
             ns.
            | 
            ns: nodesToParse copy.
            "look for type followed by an id"
            parseType: nodesToParse IfFound: [|:r. |
              resultType: r.
              parseFieldDclsIfFound: [|:r|
                ^ fb value: r.
              ].
              nodesToParse removeAll addAll: ns.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: methods\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseMethodDclIfFound: fb = ( |
             name.
            | 
            hasNodeBeforeTerminator && [nodesToParse first isJavaIdentifier]
             ifFalse: [^ self].
            name: nodesToParse removeFirst.
            parseMethodName: name IfFound: [|:r| ^ fb value: r].
            nodesToParse addFirst: name.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: methods\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseMethodName: nameID IfFound: fb = ( |
            | 
                hasNodeBeforeTerminator
            && [nodesToParse first isParenList] ifFalse: [^ self].

            errorNode: nodesToParse first.

            fb value: parseNodes methodDcl
                   copyModifiers: modifiers
                            Type: resultType
                            Name: (parseName: nameID)
                         Formals: parseFormals
                          Throws: parseThrows
                            Body: parseBody).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: methods\x7fCategory: method pieces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseName: nameID = ( |
            | 
            parseNodes attributeName copyID: nameID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: other statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseStatement = ( |
            | 
            statementParser copyParseNonEmpty: nodesToParse
                                       IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> () From: ( | {
         'Category: methods\x7fCategory: method pieces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseThrows = ( |
             body.
             r.
            | 
            "get cleaner errors for throw foo, {}"
            body: nodesToParse removeLast.

            r: throwsParser
             copyParse: nodesToParse
             AtOrAfter: errorNode
                IfFail: myFailBlock.

            nodesToParse isEmpty ifFalse: [
              failAll: nodesToParse
              Because: 'extra junk in method'
            ].
            nodesToParse add: body.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         resultType.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractNewCreatorParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser abstractNewCreatorParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (false)'
        
         isBasicType <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser abstractNewCreatorParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'Category: arrays\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseArrayCreationNewToken: n TypeNode: t IfFound: fb = ( |
             des.
            | 
            nodesToParse isEmpty not
            && [nodesToParse first isSquareList]
             ifFalse: [ ^ self]. "array"
            nodesToParse first subnodeCount = 2  ifTrue: [ 
              ^ fb value: parseArrayCreationWithInitializerNew: n Type: t
            ].
            errorNode: t.
            des: parseDimensionExpressions.
            fb value:
              parseNodes newArray copyNew: n Type: t DimensionExpressions: des).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'Category: arrays\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseArrayCreationWithInitializerNew: n Type: t = ( |
             ai.
             des.
            | 
            des: list copyRemoveAll.
            [ des addLast: parseDimensionExpression: nodesToParse removeFirst subnodes copy]
            untilFalse: [
                 nodesToParse isEmpty not
             && [nodesToParse first isSquareList
             && [nodesToParse first subnodeCount = 2]]].

            errorNode: des last.

                nodesToParse isEmpty not
            && [nodesToParse first isCurlyList]
             ifFalse: [ 
              failBecause: 'Expected array initializer in curly braces after',
                           ' or expression within' 
            ].
            ai: arrayInitializerParser 
                 copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                            IfFail: [|:e| ^ myFailBlock value: e].
            parseNodes newArray copyNew: n Type: t DimensionExpressions: des ArrayInitializer: ai).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'Category: instances\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInstanceCreationNewToken: n TypeNode: t = ( |
            | 
            nodesToParse isEmpty
            || [nodesToParse first isParenList  not] "no args"
             ifTrue: [ parseInstanceCreationWithoutArgumentsNew: n Type: t ]
              False: [ parseInstanceCreationWithArgumentsNew:    n Type: t ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'Category: instances\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInstanceCreationWithArgumentsNew: n Type: t = ( |
             args.
            | 
            t isBasicType ifTrue: [
              failNode: t
              Because: 'Expected a class type instead of a primitive type'
            ].
            nodesToParse isEmpty not
            && [ nodesToParse first isParenList ]
             ifFalse: [ failBecause: 'Expected arguments in parentheses after' ].

            args: parseArgumentList.
            errorNode: args.

            nodesToParse isEmpty not
            && [nodesToParse first isCurlyList]
             ifFalse: [
                parseNodes newInstance copyNew: n Type: t Arguments: args
             ]
             True: [ |classBody|
                classBody: classBodyParser 
                  copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                             IfFail: myFailBlock.
                parseNodes newInstanceOfAnon copyNew: n Type: t Arguments: args ClassBody: classBody
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'Category: instances\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInstanceCreationWithoutArgumentsNew: n Type: t = ( |
            | 
            t isBasicType ifTrue: [
              failAll: (n & t) asVector
              Because: 'Primitive scalars cannot be dynamically created; expected `[ expression ]\' after'
            ].
            parseNodes newInstance copyNew: n Type: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseNew = ( |
             n.
            | 
            n: nodesToParse removeFirst. "new"
            errorNode: n.
            (n isJavaKeyword: 'new') ifFalse: [
              failBecause: 'expected "new" instead of'
            ].
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseType = ( |
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'Expected type after'
            ].
            parseBasicTypeIfFound: [|:r| isBasicType: true. ^ r].
            parseBasicReferenceType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentListParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'argumentListParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'argumentListParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser argumentListParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'argumentListParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'argumentListParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser argumentListParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'argumentListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'argumentListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             argNodes.
             left.
             right.
            | 
            (nodesToParse size >= 2)
            && [nodesToParse first isOpenParen
            && [nodesToParse last isCloseParen]]
             ifFalse: [
              failAll: nodesToParse
              Because: 'Expected something in parentheses'
            ].
            left: nodesToParse removeFirst.
            errorNode: left.
            right: nodesToParse removeLast.

            argNodes: list copyRemoveAll.
            nodesToParse isEmpty ifFalse: [
              [ |:exit. e. c. |
                e: parseExpression: nodesToParse UpTo: ','.
                nodesToParse isEmpty ifTrue: [
                  argNodes addLast: parseNodes argument copyExpression: e.
                  exit value.
                ].
                c: nodesToParse removeFirst.
                c isComma ifFalse: [ failNode: c Because: 'Expected comma instead of' ].
                argNodes addLast:
                  parseNodes argument copyExpression: e Comma: c.
                errorNode: c.
              ] loopExit.
            ].
            parseNodes argumentList copyOpen: left
                                   Arguments: argNodes
                                       Close: right).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         arrayInitializerParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'arrayInitializerParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'arrayInitializerParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser arrayInitializerParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'arrayInitializerParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'arrayInitializerParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser arrayInitializerParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'arrayInitializerParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'arrayInitializerParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             close.
             open.
             r.
            | 
             open: nodesToParse removeFirst.
            close: nodesToParse removeLast.
            r: list copyRemoveAll addLast: open.
            [|:exit. comma|
              nodesToParse isEmpty ifTrue: [
                failNode: r last
                 Because: 'expected expression after'
              ].
              r addLast: variableInitializerParser
                copyParseNonEmpty: nodesToParse
                           IfFail: [|:e| ^ myFailBlock value: e].

              nodesToParse isEmpty ifTrue: exit.

              comma: nodesToParse removeFirst.
              comma isComma  ifFalse: [
                failNode: comma
                 Because: 'expected comma in array initializer instead of'
              ].
              errorNode: comma.
              r addLast: comma.
            ] loopExit.

            r addLast: close.

            parseNodes arrayInitializer copyRemoveAll addAllSubnodes: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         unorderedStatementsInCurliesParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser unorderedStatementsInCurliesParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser unorderedStatementsInCurliesParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         createResultNode = ( |
             r.
            | 
            r: resultPrototype copyRemoveAll.
            r  openCurly: nodesToParse removeFirst.
            r closeCurly: nodesToParse removeLast.
            errorNode: r openCurly.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            result: createResultNode.
            parseTopStatements.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatement: n = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseTopStatements = ( |
             treeOfTopStatements.
            | 
            treeOfTopStatements:
              statementBoundaryParser
                copyParse: nodesToParse
                AtOrAfter: result openCurly
                   IfFail: myFailBlock.

            treeOfTopStatements doSubnodes: [|:stmt|
              result addSubnode: parseStatement: stmt
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: methods\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         blockParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser unorderedStatementsInCurliesParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser blockParser.

CopyDowns:
globals javaParser unorderedStatementsInCurliesParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser blockParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseBracketsFrom: aNode IfFail: fb = ( |
            | 
            parseInnerBracketsFrom: aNode IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatement = ( |
            | 
            blockStatementParser 
              copyParseNonEmpty: nodesToParse
                         IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseTopStatements = ( |
            | 
            "override at this point because you cannot
             break block statements into statements
             before parsing each one"

            [nodesToParse isEmpty] whileFalse: [
              result addSubnode: parseStatement
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         resultPrototype = ( |
            | parseNodes block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         blockStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser blockStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser blockStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         modifiers = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInBlock = ( |
            | 
            mustNotBeEmpty.
            parseLocalVarDclStmtIfFound: [|:r| ^ r].
            parseStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInContext = ( |
            | parseInBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         breakOrContinueStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser breakOrContinueStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser breakOrContinueStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
             kw.
            | 
            kw: parseKeyword: keyword.
            nodesToParse isEmpty  ifTrue: [
              failNode: kw
               Because: 'No semicolon or identifier after'
            ].
            errorNode: nodesToParse removeFirst. "in case parseSemicolon fails"
            errorNode isSemicolon ifTrue: [
              ^ nodeProto copyKeyword: kw Semicolon: errorNode
            ].
            errorNode isJavaIdentifier ifTrue: [
              ^ nodeProto copyKeyword: kw Target: errorNode Semicolon: parseSemicolon.
            ].
            failBecause: 'Expected semicolon or identifier instead of').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         breakStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser breakOrContinueStatementParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser breakStatementParser.

CopyDowns:
globals javaParser breakOrContinueStatementParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser breakStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         keyword = 'break'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         nodeProto = ( |
            | parseNodes breakStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'breakStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: classes and interfaces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         classBodyParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser unorderedStatementsInCurliesParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classBodyParser.

CopyDowns:
globals javaParser unorderedStatementsInCurliesParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classBodyParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatement: n = ( |
            | 
            classMemberDclParser
              copyParseNonEmpty: n subnodes copy  "must have {} or ;"
                         IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classBodyParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         resultPrototype = ( |
            | parseNodes classBody).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         classMemberDclParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classMemberDclParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         modifiers.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classMemberDclParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseConstructorIfFound: fb = ( |
             nameID.
            | 
                hasNodeBeforeTerminator
            && [nodesToParse first isParenList] ifFalse: [ ^ self].

            resultType isClassOrInterfaceType  ifFalse: [
              failNode: resultType
               Because: 'constructor must be a class or interface type'
            ].
            resultType nameToken subnodeCount = 1  ifFalse: [
              failNode: resultType nameToken
               Because: 'qualified names not allowed for constructors'
            ].
            nameID: resultType nameToken firstSubnode.
            parseMethodName: nameID IfFound: [|:r|
              r body isJavaBlock ifFalse: [
                failNode: r body
                 Because: 'constructor body must be a real block, not a semicolon'
              ].
              ^ fb value: parseNodes constructorDcl copyFrom: r
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInClass = ( |
             ns.
            | 
            mustNotBeEmpty.
            ns: nodesToParse copy.
            parseModifiers.

            parseStaticInitializerIfFound: [|:r| ^ r].
            parseResultType.
            parseConstructorIfFound: [|:r| ^ r].
            parseMethodDclIfFound:   [|:r| ^ r].
            parseFieldDclsIfFound:   [|:r| ^ r].

            failAll: ns
            Because: 'Statement is not static initializer,',
                     ' constructor, method declaration, or variable declaration').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
            | parseInClass).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStaticInitializerIfFound: foundB = ( |
             block.
             static.
             unparsedBlock.
            | 

            nil = modifiers                ifTrue:  [^ self]. "no static"
            nodesToParse size = 1          ifFalse: [^ self].
            nodesToParse last isCurlyList  ifFalse: [^ self].

               ( modifiers subnodeCount = 1 )
            && [ modifiers firstSubnode isJavaKeyword: 'static' ]
             ifFalse: [
              failNode: modifiers
               Because: 'This appears to be a static initializer ',
                        'but has more modifiers than just "static"'
            ].

            static:         modifiers firstSubnode.
            unparsedBlock:  nodesToParse removeLast.

            block:
              blockParser 
                 copyParseNonEmpty: unparsedBlock subnodes copy
                            IfFail: [|:e| ^ myFailBlock value: e].

            foundB value:
              parseNodes staticInitializer
                copyStatic: static
                     Block:  block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         nameParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser nameParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         names <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser nameParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         finish = ( |
            | 
            result names: names asVector.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         isStarOK = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
            | 
            result: resultProto copyRemoveAll.
            names: list copyRemoveAll.
            [|:exit|
              parseIdentifier: exit.
              nodesToParse isEmpty  ifTrue: exit.
              parseDot: exit.
            ] loopExit.
            finish.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseDot: stopParsing = ( |
             dot.
            | 
            dot: nodesToParse first.
            dot isDot  ifFalse: stopParsing.
            nodesToParse removeFirst.

            "must look ahead, consider Pkg.ClassFoo.staticmethod"
                nodesToParse isEmpty not
            && [  |pastDot|
                   pastDot: nodesToParse first.
                   pastDot isJavaIdentifier
              || [ isStarOK && [pastDot isJavaOperator: '*']]]
             ifFalse: [
                nodesToParse addFirst: dot.
                ^ stopParsing value.
            ].
            result addSubnode: dot.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseIdentifier: stopParsing = ( |
             idOrStar.
             lastOne.
            | 
            "parse identifier or *. * must not be first name"
            nodesToParse isEmpty ifTrue: [
              failBecause: 'Missing identifier in name after'
            ].
            idOrStar: nodesToParse removeFirst.
            case if:   idOrStar isJavaIdentifier
                 Then: [lastOne: false]
                 If:   [isStarOK  && 
                       [names isEmpty not && 
                       [idOrStar isJavaOperator: '*']]]
                 Then: [lastOne: true]
                 Else: [
                         failNode: idOrStar
                          Because: 'Identifier ', 
                                   (isStarOK ifTrue: 'or asterisk ' False: ''),
                                   'expected in ', whatIAmParsing
                 ].
            names addLast: idOrStar source.
            result addSubnode: idOrStar.
            lastOne ifTrue: stopParsing.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         resultProto = ( |
            | 
            parseNodes nameNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         whatIAmParsing = 'name'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: classes and interfaces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterfaceNameParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser nameParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classOrInterfaceNameParser.

CopyDowns:
globals javaParser nameParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classOrInterfaceNameParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'nameParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         resultProto = ( |
            | 
            parseNodes classOrInterfaceName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterfaceNameParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         whatIAmParsing = 'class or interface name'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         coersionParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'coersionParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'coersionParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser coersionParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'coersionParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'coersionParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser coersionParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'coersionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'coersionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             close.
             e.
             open.
             pl.
             plns.
            | 
            open:   nodesToParse removeFirst.
            close:  nodesToParse removeLast.
            errorNode: open.
            parseType: nodesToParse
              IfFound: [|:t. c. e| 
                ^ parseNodes coersion copyOpen: open Type: t Close: close.
            ].
            e: parseExpression: nodesToParse.
            parseNodes coersion 
                copyOpen: open
              Expression: e
                   Close: close).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         compilationUnitParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser compilationUnitParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser compilationUnitParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         breakIntoTopStatements = ( |
            | 
            treeOfTopStatements:
              statementBoundaryParser
                copyParse: nodesToParse
                AtOrAfter: errorNode
                   IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         createResultNode = ( |
            | 
            parseNodes compilationUnit copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         exclude: keyword = ( |
            | 
            topStatements findFirst: [|:s. |
              s hasSubnodes  
              && [|n| n: s firstSubnode.
                   n isToken && [n source = keyword]]
            ]
            IfPresent: [ |:s| 
              failNode: s Because: 'no more ', keyword, ' statements expected'
            ]
            IfAbsent: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            breakIntoTopStatements.
            parseTopStatements.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseImportDcls = ( |
             importNodes.
            | 
            importNodes: list copyRemoveAll.
            [|:exit|
              parsePackageOrImport: parseNodes importDcl
               NameParser: nameWithStarParser
                IfPresent: [ |:impNode| importNodes addLast: impNode ]
                 IfAbsent: exit
            ] loopExit.
            result importDcls: importNodes asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parsePackageDcl = ( |
            | 
            result packageDcl:
              parsePackageOrImport: parseNodes packageDcl
               NameParser: nameParser
                IfPresent: [ |:pkgNode| pkgNode ]
                IfAbsent: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parsePackageOrImport: nodeProto NameParser: npProto IfPresent: pb IfAbsent: ab = ( |
             first.
             last.
             nameNode.
             nameNodes.
             stmt.
            | 
            topStatements isEmpty  ifTrue: [^ ab value].

            stmt: topStatements first.
            stmt hasSubnodes  ifFalse: [^ ab value].

            first: stmt firstSubnode.
            (first isJavaKeyword: nodeProto keyword)  ifFalse: [^ ab value].

            topStatements removeFirst.
            last: stmt lastSubnode.
            last isCurlyList ifTrue: [
              failAll: stmt subnodes asVector copyWithoutLast 
              Because: 'package or import statement must end with semicolon'
            ].
            last isSemicolon  ifFalse: [
              failNode: stmt Because: 'missing semicolon in package or import statement'
            ].

            nameNodes: stmt subnodes.  nameNodes removeFirst.  nameNodes removeLast.
            nameNode:  npProto
                         copyParse: nameNodes 
                         AtOrAfter: first
                            IfFail: myFailBlock.

            nameNodes isEmpty ifFalse: [
              failAll: nameNodes
              Because: 'unparsable junk found at end of ', nodeProto keyword, ' statement'
            ].

            pb value: nodeProto copyKeyword: first Name: nameNode Semi: last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseTopStatements = ( |
            | 
            topStatements: treeOfTopStatements subnodes copy asList.
            result: createResultNode.

            parsePackageDcl.  exclude: 'package'.
            parseImportDcls.  exclude: 'import'.
            parseTypeDcls.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseTypeDcl: stmt = ( |
            | 
            result typeDcls addLast: 
              compilationUnitTypeDclParser 
                copyParseNonEmpty: stmt subnodes copy "has {} or ;"
                           IfFail: myFailBlock.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseTypeDcls = ( |
            | 
            topStatements do: [|:stmt| 
              stmt isLast ifFalse: [ "ignore eofToken"
                parseTypeDcl: stmt
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         topStatements <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (parseKit parseNodes node)'
        
         treeOfTopStatements <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         compilationUnitTypeDclParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser compilationUnitTypeDclParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         modifiers.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser compilationUnitTypeDclParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         checkBody = ( |
            | 
            unparsedBody isCurlyList  ifFalse: [
                failFrom: errorNode
                      To: unparsedBody
                 Because: 'Could not find curly braces at end of ',
                          'class or interface declaration'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: testing class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         isClass = ( |
            | result isClass).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: testing class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         isInterface = ( |
            | isClass not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing class\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseAfterClassName = ( |
            | 
            removeBody.
             checkBody.
            parseSuper.
            parseInterfaces.
            parseNothingBeforeBody.
            parseClassBody).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseAfterInterfaceName = ( |
            | 
            removeBody.
             checkBody.
            parseExtendsInterfaces.
            parseNothingBeforeBody.
            parseInterfaceBody).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing class\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseClassBody = ( |
            | 
            result body:
              classBodyParser
                copyParseNonEmpty: unparsedBody subnodes copy "{} or ;"
                           IfFail: [|:e| ^ myFailBlock value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseClassOrInterfaceKeyword = ( |
             classOrInterfaceToken.
             isClass.
             n1.
            | 
            nodesToParse isEmpty ifTrue: [
              failNode: unparsedBody
               Because: 'no class or interface keyword found in type declaration before'
            ].
            n1: nodesToParse first.
            isClass: n1 isJavaKeyword: 'class'.
            isClass || [n1 isJavaKeyword: 'interface']
              ifFalse: [
                ^ failNode: n1
                   Because: 'no class or interface keyword found in type declaration before'
            ].
            nodesToParse removeFirst. "n1"

            result: isClass ifTrue: [parseNodes     classDcl copyRemoveAll]
                             False: [parseNodes interfaceDcl copyRemoveAll].
            result modifiers: modifiers.
            result classOrInterfaceToken: n1.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseClassesOrInterfacesInto: proto Keyword: extendsOrImplements OnlyOne: onlyOne = ( |
             implementsNode.
             r.
            | 

                 nodesToParse isEmpty not
            && [ nodesToParse first isJavaKeyword: extendsOrImplements]
              ifFalse: [^ proto].

            implementsNode: nodesToParse removeFirst.

            r: proto copyRemoveAll.
            r addSubnode: implementsNode.

            errorNode: implementsNode.
            [ |:exit |
              r addSubnode:
                classOrInterfaceNameParser
                  copyParse: nodesToParse
                  AtOrAfter: errorNode
                     IfFail: myFailBlock.

                 onlyOne not 
              && [nodesToParse isEmpty not
              && [nodesToParse first isComma]]
               ifFalse: exit.
              errorNode: nodesToParse removeFirst. "rm the comma"
            ] loopExit.

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseExtendsInterfaces = ( |
            | 
            result extendsInterfacesNode:
              parseClassesOrInterfacesInto: result extendsInterfacesNode
                Keyword: 'extends'
                OnlyOne: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInCompilationUnit = ( |
            | 
            parseEmptyStatementIfFound: [|:r| ^ r].
            parseModifiers.
            parseClassOrInterfaceKeyword.
            parseSimpleName.
            isClass ifTrue: [ parseAfterClassName    ]
                     False: [ parseAfterInterfaceName].
            result modifiers: modifiers.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInContext = ( |
            | 
            parseInCompilationUnit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInterfaceBody = ( |
            | 
            result body:
              interfaceBodyParser
                copyParseNonEmpty: unparsedBody subnodes copy "must have open curly"
                           IfFail: [|:e| ^ myFailBlock value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing class\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInterfaces = ( |
            | 
            result implementsInterfacesNode:
              parseClassesOrInterfacesInto: result implementsInterfacesNode
                Keyword: 'implements'
                OnlyOne: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseNothingBeforeBody = ( |
            | 
            nodesToParse isEmpty ifFalse: [ 
              failAll: nodesToParse
              Because: 'Unparsable junk in ', 
                       result classOrInterface, ' declaration'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing class\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseSuper = ( |
            | 
            result extendsClassNode:
              parseClassesOrInterfacesInto: result extendsClassNode
                Keyword: 'extends'
                OnlyOne: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> 'parent' -> () From: ( | {
         'Category: parsing either class or interface\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         removeBody = ( |
             noBody = 'no body (semicolon or block) found after:'.
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: noBody
            ].
                nodesToParse last isCurlyList
            || [nodesToParse last isSemicolon]
             ifFalse: [
              failNode: nodesToParse last  Because: noBody
            ].
            unparsedBody: nodesToParse removeLast).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnitTypeDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         unparsedBody.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         continueStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser breakOrContinueStatementParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser continueStatementParser.

CopyDowns:
globals javaParser breakOrContinueStatementParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser continueStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         keyword = 'continue'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         nodeProto = ( |
            | 
            parseNodes continueStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'continueStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'breakOrContinueStatementParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         doStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser doStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser doStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseDoStatement = ( |
             do.
             exprInParens.
             semi.
             stmt.
             while.
            | 
            do: parseKeyword: 'do'.

            nodesToParse isEmpty ifTrue: [
              failNode: do Because: 'No statement after'
            ].
            stmt: parseStatement.

            errorNode: stmt.
            while: parseKeyword: 'while'.

            errorNode: while.
            exprInParens: parseExpressionParenList.

            errorNode: exprInParens.
            semi: parseSemicolon.

            parseNodes doStatement
              copyDo: do
               Statement: stmt
               While: while
               ExpressionInParens: exprInParens
               Semicolon: semi).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'doStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
            | parseDoStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         emptySquaresParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'emptySquaresParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'emptySquaresParser' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser emptySquaresParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'emptySquaresParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'emptySquaresParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser emptySquaresParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'emptySquaresParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'emptySquaresParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             listOSquares.
            | 
            listOSquares: list copyRemoveAll.
            [nodesToParse isEmpty not && [nodesToParse first isSquareList]]
             whileTrue: [ | squares |
              squares: nodesToParse removeFirst.
              squares subnodeCount = 2  ifFalse: [
                failNode: squares Because: '[] should be empty' 
              ].
              listOSquares addLast: squares.
            ].
            listOSquares).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         expression1Parser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser expression1Parser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser expression1Parser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         matchQuestionMark: q From: from To: to = ( |
            | 
            [|n|
              from isEmpty ifTrue: [
                failNode: q
                 Because: 'Could not find colon to match with question mark'
              ].
              n: from first.
              case 
                if: [ n isColon ] Then: [ ^ self ]
                If: [ n isJavaOperator: '?' ]
                Then: [ to addLast: from removeFirst. "?"
                        matchQuestionMark: n
                                     From: from
                                       To: to.
                        to addLast: from removeFirst. ":"
                ]
                Else: [ to addLast: from removeFirst ].
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression1Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             c.
             e1.
             e2.
             e3.
             ns.
             q.
            | 
            e1: parseExpression2.
            nodesToParse isEmpty not
            && [nodesToParse first isJavaOperator: '?']
             ifFalse: [^ e1].

            q: nodesToParse removeFirst.

            errorNode: q.
            ns: nodesToParse copyRemoveAll.
            matchQuestionMark: q From: nodesToParse To: ns.
            e2: parseExpression: ns.

            nodesToParse isEmpty not
            && [nodesToParse first isJavaOperator: ':']
             ifFalse: [ failNode: e2 Because: 'Colon expected after'].

            c: nodesToParse removeFirst.
            errorNode: c.
            e3: parseExpression1.

            parseNodes expressionIf
              copyE1: e1 Question: q E2: e2 Colon: c E3: e3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         expression2Parser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression2Parser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression2Parser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser expression2Parser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression2Parser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression2Parser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser expression2Parser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression2Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression2Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             r.
            | 
            r: parseExpression3.
            nodesToParse isEmpty  ifTrue: [^ r].
            nodesToParse first isJavaInfixOperator  ifTrue: [
              [
                r: parseNodes infixExpression 
                     copyExpression: r
                           Operator: nodesToParse removeFirst
                         Expression: parseExpression3.
              ] untilFalse: [ 
                     nodesToParse isEmpty not
                && [ nodesToParse first isJavaInfixOperator ]].
              ^ r
            ].
            (nodesToParse first isJavaKeyword: 'instanceof')  ifTrue: [
              errorNode: nodesToParse removeFirst.
              ^ parseNodes infixExpression
                copyExpression: r
                      Operator: errorNode
                    Expression: parseType
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         expression3Parser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser expression3Parser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser expression3Parser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
            | 
            nodesToParse isEmpty ifTrue: [ ^ parsePrimary ].
            parsePrefixOpIfFound: [|:r| ^ r].
            parseCoersionIfFound: [|:r| ^ r].
            parsePrimaryCase).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseCoersionIfFound: fb = ( |
             c.
             isCoersion <- bootstrap stub -> 'globals' -> 'false' -> ().
             t.
            | 
            nodesToParse first isParenList ifFalse: [^ self].
            "figure out if this is a coersion or a parenthesized expression"
            t: nodesToParse asVector at: 1 IfAbsent: [^ self].
             "++, --: could be (foobar)++ or (foobar)++snort"
            isCoersion:
             case
              if:   [ t isDot                    ]  Then: false
              If:   [ t isJavaOperator not       ]  Then: true
              If:   [ t isJavaPostfixOperator    ]  Then: [nodesToParse size > 2]
              If:   [ t isJavaInfixOperator      ]  Then: false
              If:   [ t isJavaAssignmentOperator ]  Then: false
              If:   [ t isJavaPrefixOperator     ]  Then: true.

            isCoersion ifFalse: [^ self].

            c: coersionParser copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                                         IfFail: [|:e| ^ myFailBlock value: e].
            fb value:
              parseNodes prefixExpression
                copyOperator: c Expression: parseExpression3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseDotSelector = ( |
             dot.
             expected = 'Expected identifier, `this\', `super\', or `new\' '.
             n.
            | 
            dot: nodesToParse removeFirst.
            errorNode: dot.
            nodesToParse isEmpty ifTrue: [
              failBecause: expected, 'after'.
            ].
            n: nodesToParse removeFirst.
            errorNode: n.
            case
              if: [ n isJavaIdentifier       ] Then: [ parseSelectorDot: dot ID:    n ]
              If: [ n isJavaKeyword: 'this'  ] Then: [ parseSelectorDot: dot This:  n ]
              If: [ n isJavaKeyword: 'super' ] Then: [ parseSelectorDot: dot Super: n ]
              If: [ n isJavaKeyword: 'new'   ] Then: [ parseSelectorDot: dot New:   n ]
              Else: [ failBecause: expected, 'instead of' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parsePrefixOpIfFound: fb = ( |
             e.
             op.
            | 
            nodesToParse first isJavaPrefixOperator ifFalse: [^self].
            op: nodesToParse removeFirst.
            errorNode: op.
            e: parseExpression3.
            fb value: 
              parseNodes prefixExpression 
                copyOperator: op
                  Expression: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parsePrimaryCase = ( |
             post.
             r.
             sel.
            | 
            r: parsePrimary.
            parseSelectorIfFound: [|:s|
              r: parseNodes postfixExpression copyExpression: r Operator: s
            ].
            [nodesToParse isEmpty not && [nodesToParse first isJavaPostfixOperator]]
             whileTrue: [
              r: parseNodes postfixExpression
                   copyExpression: r Operator: nodesToParse removeFirst
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSelectorDot: dot ID: id = ( |
            | 
                 nodesToParse isEmpty not
            && [ nodesToParse first isParenList ]
             ifTrue: [ parseNodes messageSelector copyDot: dot Name: id Arguments: parseArgumentList ]
              False: [ parseNodes   fieldSelector copyDot: dot Name: id ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSelectorDot: dot New: n = ( |
             c.
            | 
            nodesToParse addFirst: n.
            c:  newInnerCreatorParser 
                  copyParseNonEmpty: nodesToParse
                             IfFail: [|:e| ^ myFailBlock value: e].
            parseNodes newInnerCreatorSelector
                  copyDot: dot
                  Creator: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSelectorDot: dot Super: s = ( |
             args.
             d.
             id.
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'Expected either arguments in parentheses or period after'
            ].
            nodesToParse first isParenList ifTrue: [
              ^ parseNodes messageSelector copyDot: dot Name: s Arguments: parseArgumentList
            ].
            nodesToParse first isDot  ifFalse: [
              failNode: nodesToParse first
               Because: 'Expected either arguments in parentheses or period instead of'
            ].
            d: nodesToParse removeFirst.
            nodesToParse isEmpty not && [nodesToParse first isJavaIdentifier]
             ifFalse: [ failNode: d Because: 'Expected identifier after' ].
            id: nodesToParse removeFirst.
            nodesToParse isEmpty not && [nodesToParse first isParenList]
             ifFalse: [
              ^ parseNodes qualifiedSuperSelector copyDot: dot Super: s Dot: d Name: id
            ].
            args: parseArgumentList.
            parseNodes qualifiedSuperInvocationSelector
              copyDot: dot Super: s Dot: d Name: id Arguments: args).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSelectorDot: dot This: t = ( |
            | 
            parseNodes fieldSelector copyDot: dot Name: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSelectorIfFound: fb = ( |
            | 
            case if:   [ nodesToParse isEmpty ]             Then: [^ self]
                 If:   [ nodesToParse first isDot ]         Then: [^ fb value: parseDotSelector ]
                 If:   [ nodesToParse first isSquareList ]  Then: [^ fb value: parseSquareSelector ]
                 Else: [ ^ self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expression3Parser' -> 'parent' -> () From: ( | {
         'Category: selectors\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSquareSelector = ( |
             e.
             lft.
             rt.
             sql.
             sqlns.
            | 
            sql: nodesToParse removeFirst.
            sqlns: sql subnodes copy.
            lft: sqlns removeFirst.
            rt:  sqlns removeLast.
            e: expressionParser copyParse: sqlns AtOrAfter: lft IfFail: [|:e| ^ myFailBlock value: e].
            sqlns isEmpty ifFalse: [failNode: e Because: 'Extra junk after'].
            parseNodes arrayAccess copyLeft: lft Expression: e Right: rt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser expressionParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser expressionParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'expression expected after'
            ].
                nodesToParse first isSemicolon
            || [nodesToParse first isComma]
             ifTrue: [
              failNode: nodesToParse first
               Because: 'expected expression before'
            ].
            parseExpression).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpression = ( |
            | 
            parseExpressionRightToLeft: parseExpression1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseExpressionRightToLeft: e1 = ( |
             op.
            | 
            nodesToParse isEmpty not
            && [ nodesToParse first isJavaAssignmentOperator ]
              ifFalse: [^ e1].

            op: nodesToParse removeFirst.
            errorNode: op.

            parseNodes infixExpression
              copyExpression:  e1
                    Operator:  op
                  Expression:  parseExpressionRightToLeft: parseExpression1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         forStatementParenListParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser forStatementParenListParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser forStatementParenListParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         modifiers = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             close.
             cond.
             init.
             open.
             update.
            | 
            open:  nodesToParse removeFirst.  errorNode: open.
            close: nodesToParse removeLast.

            init:     parseInit.    errorNode: init.
            cond:     parseCond.    errorNode: cond. 
            update:   parseUpdate.  errorNode: update.

            nodesToParse isEmpty ifFalse: [
              failAll: nodesToParse
              Because: 'Extra junk found at end of for statement increment'
            ].
            parseNodes forParenList copyOpen: open Init: init Cond: cond UpdateOrNil: update Close: close).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseCond = ( |
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'Missing semicolon in for statement after'
            ].
            nodesToParse first isSemicolon ifTrue: [
              ^ parseNodes forCond copySemicolon: nodesToParse removeFirst
            ].
            parseNodes forCond copyExpression: (parseExpression: nodesToParse UpTo: ';')
                                    Semicolon: parseSemicolon).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInit = ( |
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'Missing semicolon in for statement after'
            ].
            nodesToParse first isSemicolon ifTrue: [
              ^ parseNodes forInit copySemicolon: nodesToParse removeFirst
            ].
            parseLocalVarDclStmtIfFound: [|:r| ^ parseNodes forInit copyLocalVarDclStmt: r].
            parseNodes forInit copyExpressionList: parseStatementExpressionList
                                        Semicolon: parseSemicolon).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParenListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseUpdate = ( |
            | 
            nodesToParse isEmpty ifTrue: [ ^ nil ].
            parseNodes forUpdate copyExpressionList: parseStatementExpressionList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         forStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser forStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser forStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             for.
             forParenList.
             stmt.
            | 
            for: parseKeyword: 'for'.
            errorNode: for.

            forParenList: parseParenList.
            errorNode: forParenList.

            nodesToParse isEmpty ifTrue: [
              failBecause: 'Expected statement to iterate in for-statement after'
            ].
            stmt: parseStatement.

            parseNodes forStatement
                copyFor: for
              ParenList: forParenList
              Statement: stmt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'forStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseParenList = ( |
             pl.
             plNodes.
            | 
            nodesToParse isEmpty not
            && [nodesToParse first isParenList]
              ifFalse: [ failBecause: 'expected open parenthesis after'].
            pl: nodesToParse removeFirst.

            forStatementParenListParser
              copyParseNonEmpty: pl subnodes copy
                         IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: methods\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         formalParameterListParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser formalParameterListParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser formalParameterListParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             formals.
             leftParen.
             parenList.
             rightParen.
            | 
             leftParen: nodesToParse removeFirst.
            rightParen: nodesToParse removeLast.
            formals: parseFormals.
            parseNodes formalParameterList
              copyLeft: leftParen
               Formals: formals
                 Right: rightParen).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterListParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseFormals = ( |
             r.
            | 
            r: list copyRemoveAll.
            nodesToParse isEmpty ifTrue: [^ r].
            errorNode: nodesToParse first.
            [| comma |
              r addLast:
                formalParameterParser
                  copyParse: nodesToParse
                  AtOrAfter: errorNode
                     IfFail: [|:e| ^ myFailBlock value: e].
              nodesToParse isEmpty ifTrue: [^ r].
              comma: nodesToParse removeFirst.
              errorNode: comma.
              comma isComma  ifFalse: [
                failBecause: 'Just parsed a formal parameter so this should have been a comma'
              ].
              r last comma: comma.
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: methods\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         formalParameterParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser formalParameterParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser formalParameterParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'formalParameterParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             identifier.
             squares.
             type.
            | 
            type: typeParser 
                    copyParse: nodesToParse
                    AtOrAfter: errorNode
                       IfFail: myFailBlock.
            nodesToParse isEmpty ifTrue: [
              failNode: type
               Because: 'no formal paramater name found after its type'
            ].
            identifier: nodesToParse removeFirst.
            identifier isJavaIdentifier ifFalse: [
              failNode: identifier
               Because: 'formal paramater name must be an identifier'
            ].
            squares: emptySquaresParser
                       copyParse: nodesToParse
                       AtOrAfter: identifier
                          IfFail: myFailBlock.

            parseNodes formalParameter
              copyType: type Name: identifier Squares: squares).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: classes and interfaces\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         interfaceBodyParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser unorderedStatementsInCurliesParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser interfaceBodyParser.

CopyDowns:
globals javaParser unorderedStatementsInCurliesParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser interfaceBodyParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'unorderedStatementsInCurliesParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatement: n = ( |
            | 
            interfaceMemberDclParser
              copyParseNonEmpty: n subnodes copy
                         IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceBodyParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         resultPrototype = ( |
            | parseNodes interfaceBody).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         interfaceMemberDclParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser interfaceMemberDclParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         modifiers.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser interfaceMemberDclParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         mustBeAbstract: aMethodDcl = ( |
            | 
            aMethodDcl body isSemicolon  ifFalse: [
              failNode: aMethodDcl body
               Because: 'method declaration in interface must be abstract and have ";" instead of'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         mustBeConstant: aVarDclsStatement = ( |
            | 
               ( modifiers != nil )
            && [ modifiers subnodes anySatisfy: [|:n| n source = 'final']]
             ifFalse: [
               failNode: aVarDclsStatement
                Because: 'variable declaration in interface must be constant but has no "final" modifier'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
            | parseInInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'interfaceMemberDclParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInInterface = ( |
             ns.
            | 
            mustNotBeEmpty.
            ns: nodesToParse copy.
            parseModifiers.
            parseResultType.
            parseMethodDclIfFound:   [|:r| mustBeAbstract: r. ^ r].
            parseFieldDclsIfFound:   [|:r| mustBeConstant: r. ^ r].

            failAll: ns
            Because: 'Statement is not method or variable declaration').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         modifiersParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'modifiersParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'modifiersParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser modifiersParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'modifiersParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'modifiersParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser modifiersParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'modifiersParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'modifiersParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             r.
            | 
            r: parseNodes modifiers copyRemoveAll.
            [| :exit. n |
              nodesToParse isEmpty ifTrue: exit.
              n: nodesToParse first.
              n isJavaModifier ifFalse: exit.
              r addSubnode: nodesToParse removeFirst.
            ] loopExit.
            r hasSubnodes ifTrue: [r] False: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         nameWithStarParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameWithStarParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'isStarOK' From:
             globals javaParser nameParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameWithStarParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser nameWithStarParser.

CopyDowns:
globals javaParser nameParser. copy 
SlotsToOmit: isStarOK.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'nameWithStarParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (true)'
        
         isStarOK = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         newCreatorParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'newCreatorParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNewCreatorParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'newCreatorParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser newCreatorParser.

CopyDowns:
globals javaParser abstractNewCreatorParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'newCreatorParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'newCreatorParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser newCreatorParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'newCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'newCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             des.
             n.
             t.
            | 
            n: parseNew.
            t: parseType.
               parseArrayCreationNewToken: n TypeNode: t IfFound: [|:r| ^ r].
            parseInstanceCreationNewToken: n TypeNode: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         newInnerCreatorParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'newInnerCreatorParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNewCreatorParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'newInnerCreatorParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser newInnerCreatorParser.

CopyDowns:
globals javaParser abstractNewCreatorParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'newInnerCreatorParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'newInnerCreatorParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser newInnerCreatorParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'newInnerCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNewCreatorParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'newInnerCreatorParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             des.
             n.
             t.
            | 
            n: parseNew.
            t: parseType.
            t nameToken names size > 1  ifTrue: [
              failNode: t Because: 'Must be simple identifier instead of'
            ].
            errorNode: t.
            parseInstanceCreationWithArgumentsNew: n Type: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         primaryParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser primaryParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser primaryParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             n.
             w1 = 'Expected a parenthesized expression, \"this\", \"super\", a literal, '.
             w2 = '\"new\", an identifier, a basic type, or \"void\" '.
            | 
            nodesToParse isEmpty ifTrue: [failBecause: w1, w2, 'after'].

            n: nodesToParse first.

            case
              if: [n isParenList           ] Then: [ parseExpressionParenList]
              If: [n isJavaKeyword: 'this' ] Then: [ parseThisPrimary ]
              If: [n isJavaKeyword: 'super'] Then: [ parseSuperPrimary ]
              If: [n isLiteral             ] Then: [ nodesToParse removeFirst ]
              If: [n isJavaKeyword: 'new'  ] Then: [ parseNewCreator ]
              If: [n isJavaIdentifier      ] Then: [ parseIdentifierPrimary ]
              If: [n isJavaTypeKeyword     ] Then: [ parseBasicTypePrimary ]
              If: [n isJavaKeyword: 'void' ] Then: [ parseVoidPrimary]

              Else: [ failNode: n Because: w1, w2, 'instead of' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'Category: identifier\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseArrayClassAccessName: n DimExpr: de = ( |
             class.
             des.
             dot.
            | 
            des: list copyRemoveAll addLast: de.
            [nodesToParse isEmpty not
             && [nodesToParse first isSquareList
             && [nodesToParse first subnodeCount = 2]]
            ] whileTrue: [ 
              des addLast: parseDimensionExpression:
                nodesToParse removeFirst subnodes copy
            ].
            errorNode: des last.
            nodesToParse isEmpty not
            && [ nodesToParse first isDot
            && [
                dot: nodesToParse removeFirst.
                nodesToParse isEmpty not
            && [nodesToParse first isJavaKeyword: 'class']]]
             ifFalse: [
              failBecause: 'Expected expression inside or `.class\' after'
            ].
            class: nodesToParse removeFirst.
            parseNodes dotClassType
              copyPreDot: (parseNodes arrayClass copyName: n DEs: des)
                     Dot: dot 
                   Class: class).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'Category: identifier\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseArrayOrArrayClassAccessName: n = ( |
             de.
            | 
            de: parseDimensionExpression:
               nodesToParse removeFirst subnodes copy.
            de hasExpression
             ifFalse: [ parseArrayClassAccessName:       n DimExpr: de ]
                True: [ parseNodes arrayAccess copyName: n DimExpr: de ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseBasicTypePrimary = ( |
             cl.
             dot.
             t.
            | 
            t: arrayify: parseBasicTypeIfFound: [|:r| r].
            errorNode: t.
            case
                 if: [nodesToParse isEmpty]
               Then: [failBecause: '`.class\' expected after']
                 If: [dot: nodesToParse removeFirst. dot isDot not]
               Then: [failNode: dot Because: '`.class\' expected instead of']
                 If: [nodesToParse isEmpty
                 ||  [cl: nodesToParse removeFirst. (cl isJavaKeyword: 'class') not]]
               Then: [failNode: dot Because: '`class\' expected after']
               Else: [parseNodes dotClassType copyPreDot: t Dot: dot Class: cl]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'Category: identifier\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseIdentifierPrimary = ( |
             name.
            | 
            name: parseQualifiedIdentifier.
            nodesToParse isEmpty ifTrue: [^ name].
            errorNode: name.
            nodesToParse first isSquareList ifTrue: [
              ^ parseArrayOrArrayClassAccessName: name
            ].
            nodesToParse first isParenList ifTrue: [
              ^ parseNodes methodInvocation copyName: name Arguments: parseArgumentList
            ].
            nodesToParse first isDot ifTrue: [|d|
              d: nodesToParse removeFirst.
              nodesToParse isEmpty ifTrue: [
                failNode: d
                 Because: 'Expected "class" "this" "super" or "new" after'
              ].
              ^ parseSpecialIDPrimaryName: name Dot: d KW: nodesToParse removeFirst.
            ].
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseNewCreator = ( |
            | 
            newCreatorParser copyParseNonEmpty: nodesToParse IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'Category: identifier\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseSpecialIDPrimaryName: n Dot: d KW: k = ( |
            | 
            case
              if:   [ k isJavaKeyword: 'class']
              Then: [ parseNodes dotClassType copyPreDot: n Dot: d Class: k]

              If:   [ k isJavaKeyword: 'this']
              Then: [ parseNodes specialIDPrimary copyName: n Dot: d KW: k]

              If:   [ k isJavaKeyword: 'super']
              Then: [ |a|
                      nodesToParse isEmpty not
                      && [nodesToParse first isParenList]
                       ifFalse: [
                         failNode: k 
                          Because: 'Expected arguments in parentheses after'
                      ].
                      a: parseArgumentList.
                      parseNodes namedSuperInvocation
                        copyName: n Dot: d Super: k Arguments: a]

              If:   [ k isJavaKeyword: 'new']
              Then: [ |c|
                      nodesToParse addFirst: k.
                      c:  newInnerCreatorParser 
                            copyParse: nodesToParse
                            AtOrAfter: k
                               IfFail: [|:e| ^ myFailBlock value: e].
                      parseNodes newInnerCreator
                           copyName: n
                                Dot: d
                        NewInstance: c
              ]
              Else: [
                failNode: k 
                 Because: 'Expected `class\', `this\', `super\', `new\' after'
              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseSuperPrimary = ( |
             args.
             dot.
             id.
             s.
            | 
            s: nodesToParse removeFirst.

            nodesToParse isEmpty  ifTrue: [
              failNode: s Because: 'Expected either a period or arguments in parentheses after'
            ].
            nodesToParse first isParenList ifTrue: [
              errorNode: s.
            ^ parseNodes superInvocation copySuper: s Arguments: parseArgumentList
            ].
            nodesToParse first isDot
             ifFalse: [
              failNode: nodesToParse first
               Because: 'Expected either a period or arguments in parentheses instead of'
            ].
            dot: nodesToParse removeFirst.
            nodesToParse isEmpty ifTrue: [
              failNode: dot Because: 'Expected identifier after'
            ].
            nodesToParse first isJavaIdentifier  ifFalse: [
              failNode: nodesToParse first Because: 'Expected identifier instead of'
            ].
            id: nodesToParse removeFirst.
            nodesToParse isEmpty not
            && [nodesToParse first isParenList]  ifFalse: [
              ^ parseNodes superFieldAccess copySuper: s Field: id
            ].
            parseNodes superInvocation copySuper: s Field: id Arguments: parseArgumentList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseThisPrimary = ( |
             a.
             t.
            | 
            t: nodesToParse removeFirst.
            nodesToParse isEmpty not
            && [nodesToParse first isParenList]
              ifFalse: [^ t].
            parseNodes thisInvocation copyThis: t Arguments: parseArgumentList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'primaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseVoidPrimary = ( |
             class.
             dot.
             void.
            | 
            void: nodesToParse removeFirst.
            nodesToParse isEmpty not
            && [dot: nodesToParse removeFirst. dot isDot]
             ifFalse: [ failNode: void Because: 'Expected `.class\' after' ].
            nodesToParse isEmpty not
            && [class: nodesToParse removeFirst. class isJavaKeyword: 'class']
             ifFalse: [ failNode: dot Because: 'Expected `class\' after' ].

            parseNodes dotClassType copyPreDot: void Dot: dot Class: class).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         qualifiedIdentifierParser = ( |
            | nameParser).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         returnStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser returnStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser returnStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
            | parseReturnStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'returnStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseReturnStatement = ( |
             r.
            | 
            r: parseKeyword: 'return'.
            errorNode: r.
            nodesToParse isEmpty ifTrue: [
              failBecause: 'no semicolon or expression found after'
            ].
            nodesToParse first isSemicolon ifTrue: [
              parseNodes returnStatement
                copyReturn: r
                 Semicolon: parseSemicolon
            ]
            False: [
              parseNodes returnStatement
                copyReturn: r
                Expression: (parseExpression: nodesToParse UpTo: ';')
                 Semicolon: parseSemicolon
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceToNoncommentParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'sourceToNoncommentParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit sourceToNoncommentParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'sourceToNoncommentParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser sourceToNoncommentParser.

CopyDowns:
globals parseKit sourceToNoncommentParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'sourceToNoncommentParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser sourceToNoncommentParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         nameSpace* = bootstrap stub -> 'globals' -> 'javaParser' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'sourceToNoncommentParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'sourceToNoncommentParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         startsWithModifiersParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser startsWithModifiersParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (parseKit parseNodes node)'
        
         modifiers <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser startsWithModifiersParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         mustNotHaveModifiers: what = ( |
            | 
            modifiers = nil  ifFalse: [
              failNode: modifiers
               Because: what, ' should not have modifiers'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'startsWithModifiersParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
             r.
            | 
            parseModifiers.
            r: parseAfterModifiers.
            nil = modifiers ifFalse: [r modifiers: modifiers].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         statementBoundaryParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> () From: ( |
             {} = 'Comment: Java statements either end in semicolons or
curly blocks.\x7fModuleInfo: Creator: globals javaParser statementBoundaryParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         nodesInStatement.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser statementBoundaryParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         finish = ( |
            | 
            nodesInStatement isEmpty ifTrue: [^self].

               (nodesInStatement size = 1)
            && [nodesInStatement first isToken
            && [nodesInStatement first isLast "eofToken"]]
              ifTrue: [ 
                result addSubnode: nodesInStatement first.
              ^ self
            ].

            failAll: nodesInStatement
            Because: 'Statement must end with either semicolon or curly braces').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parse = ( |
            | 
            result: parseKit parseNodes node copyRemoveAll.
            nodesInStatement: list copyRemoveAll.

            nodesToParse do: [|:n| parseNode: n].
            finish.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseNode: n = ( |
            | 
               (n isCurlyList && [priorWasEquals not])
            || [n isSemicolon]
              ifTrue: [ parseStatementEnd: n ]
               False: [ parseOther: n ].
            priorWasEquals:  n isJavaOperator: '='.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseOther: n = ( |
            | nodesInStatement addLast: n. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatementEnd: n = ( |
             s.
            | 
            nodesInStatement addLast: n.
            s: node copyRemoveAll.
            s addAllSubnodes: nodesInStatement.
            result addSubnode: s.
            nodesInStatement removeAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementBoundaryParser' -> () From: ( | {
         'Comment: To handle 
\"a = {3};\" case, ignore {} after an =\x7fModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         priorWasEquals <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         statementExpressionParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementExpressionParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser expressionParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementExpressionParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser statementExpressionParser.

CopyDowns:
globals javaParser expressionParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementExpressionParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementExpressionParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser statementExpressionParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementExpressionParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'expressionParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         statementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser statementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser statementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseForStatementIfFound: fb = ( |
            | 
            nodesToParse isEmpty not
            && [nodesToParse first isJavaKeyword: 'for']
              ifFalse: [^ self].

            fb value:
              forStatementParser copyParseNonEmpty: nodesToParse IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseIfStatementIfFound: fb = ( |
             condInParens.
             else.
             elseKW.
             if.
             then.
            | 
            nodesToParse isEmpty not
            && [ nodesToParse first isJavaKeyword: 'if']
             ifFalse: [^ self].
            if: nodesToParse removeFirst.

            errorNode: if.
            condInParens: parseExpressionParenList.

            errorNode: condInParens.
            then: parseStatement.

            errorNode: then.
            nodesToParse isEmpty not
            && [ nodesToParse first isJavaKeyword: 'else']
             ifFalse: [
              ^ fb value: parseNodes ifStatement 
                  copyIf: if ConditionInParens: condInParens Then: then
            ].
            elseKW: nodesToParse removeFirst.

            errorNode: elseKW.
            else: parseStatement.

            fb value: parseNodes ifStatement
              copyIf: if ConditionInParens: condInParens Then: then 
                               ElseKeyword: elseKW ElseStatement: else).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
            | parseStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseLabels = ( |
             r.
            | 
            r: list copyRemoveAll.
            [|:exit. id. colon|
              nodesToParse isEmpty not && [ nodesToParse first isJavaIdentifier ]
               ifFalse: exit.
              id: nodesToParse removeFirst.
              nodesToParse isEmpty not && [ nodesToParse first isColon ]
               ifFalse: [ nodesToParse addFirst: id.  exit value ].
              r addLast: parseNodes label copyIdentifier: id Colon: nodesToParse removeFirst.
            ] loopExit.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatement = ( |
             labels.
             stmt.
            | 
            labels: parseLabels.
            stmt: parseStatementAfterLabels.
            stmt addLabels: labels.
            stmt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatementAfterLabels = ( |
            | 
            parseIfStatementIfFound:    [|:r| ^ r].
            parseWhileStatementIfFound: [|:r| ^ r].
            parseForStatementIfFound:   [|:r| ^ r].

            statementWithoutTrailingSubstatementParser
              copyParse: nodesToParse
              AtOrAfter: errorNode
                 IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseWhileStatementIfFound: fb = ( |
             exprInParens.
             stmt.
             while.
            | 
            nodesToParse isEmpty not
            && [ nodesToParse first isJavaKeyword: 'while' ]
             ifFalse: [ ^ self ].
            while: nodesToParse removeFirst.

            errorNode: while.
            exprInParens: parseExpressionParenList.

            errorNode: exprInParens.
            stmt: parseStatement.

            fb value: parseNodes whileStatement
              copyWhile: while
                ConditionInParens: exprInParens
                Statement: stmt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         statementWithoutTrailingSubstatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser statementWithoutTrailingSubstatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         modifiers.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser statementWithoutTrailingSubstatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseBreakStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'break']
                ParseWith: breakStatementParser
                  IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseContinueStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'continue']
                ParseWith: continueStatementParser
                  IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseDoStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'do']
                ParseWith: doStatementParser
                  IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseExpressionStatementIfFound: fb = ( |
             e.
             s.
            | 
            nodesToParse isEmpty ifTrue: [^ self].
            nodesToParse first couldStartJavaExpression ifFalse: [^ self].
            e: parseStatementExpression.
            errorNode: e.
            s: parseSemicolon.
            fb value: parseNodes expressionStatement copyExpression: e Semicolon: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInContext = ( |
            | 
            parseInStatementWithoutTrailingSubstatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInStatementWithoutTrailingSubstatement = ( |
             expectedWhat = 'Expected {, ;, expression, switch, do, break, continue, return, throw or try'.
            | 
            "synchronized is a modifier"
            parseSynchronizedStatementIfFound:  [|:r| ^ r].
            parseModifiers.
            mustNotHaveModifiers: 'statement in block'.

            nodesToParse isEmpty ifTrue: [
              failBecause: expectedWhat, ' after'
            ].
            parseNestedBlockIfFound:            [|:r| ^ r].
            parseEmptyStatementIfFound:         [|:r| ^ r].
            parseSwitchStatementIfFound:        [|:r| ^ r].
            parseDoStatementIfFound:            [|:r| ^ r].
            parseBreakStatementIfFound:         [|:r| ^ r].
            parseContinueStatementIfFound:      [|:r| ^ r].
            parseReturnStatementIfFound:        [|:r| ^ r].
            parseThrowStatementIfFound:         [|:r| ^ r].
            parseTryStatementIfFound:           [|:r| ^ r].
            parseExpressionStatementIfFound:    [|:r| ^ r].

            failAll: nodesToParse
            Because: expectedWhat, ' instead of').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseNestedBlockIfFound: fb = ( |
            | 
            nodesToParse isEmpty not && [ nodesToParse first isCurlyList ]
              ifFalse: [ ^ self].

            fb value:  
              parseNodes blockStatement copyBlock:
                blockParser copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                                       IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseReturnStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'return' ]
               ParseWith:  returnStatementParser
                 IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSwitchStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'switch']
                ParseWith: switchStatementParser
                  IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSynchronizedStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'synchronized']
                ParseWith: synchronizedStatementParser
                  IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseThrowStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'throw' ]
               ParseWith:  throwStatementParser
                 IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'statementWithoutTrailingSubstatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseTryStatementIfFound: fb = ( |
            | 
            ifFirstNodeIs: [|:n| n isJavaKeyword: 'try']
                ParseWith: tryStatementParser
                  IfFound: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         switchBlockParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser blockParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser switchBlockParser.

CopyDowns:
globals javaParser blockParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser switchBlockParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'blockParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseStatement = ( |
            | 
            switchBlockStatementParser 
              copyParseNonEmpty: nodesToParse
                         IfFail: myFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         resultPrototype = ( |
            | 
            parseNodes switchBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         switchBlockStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser blockStatementParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser switchBlockStatementParser.

CopyDowns:
globals javaParser blockStatementParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser switchBlockStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'blockStatementParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseCaseIfFound: fb = ( |
             caseKW.
             colon.
             expr.
            | 
            nodesToParse isEmpty not
            && [nodesToParse first isJavaKeyword: 'case']
              ifFalse: [^ self].
            caseKW: nodesToParse removeFirst.

            errorNode: caseKW.
            expr: parseExpression: nodesToParse UpTo: ':'.

            nodesToParse isEmpty not
            && [nodesToParse first isColon]
             ifFalse: [
              failAll: (caseKW & expr) asVector
              Because: 'No colon found after'
            ].
            colon: nodesToParse removeFirst.

            fb value:
              parseNodes caseLabel copyCase: caseKW
                                   Expression: expr
                                   Colon: colon).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseDefaultIfFound: fb = ( |
             c.
             d.
            | 
            nodesToParse isEmpty ifTrue: [^ self].
            (nodesToParse first isJavaKeyword: 'default')
              ifFalse: [^ self].
            d: nodesToParse removeFirst.

            nodesToParse isEmpty not
            && [nodesToParse first isColon]
            ifFalse: [
              failNode: d
               Because: 'No colon found after'
            ].
            c: nodesToParse removeFirst.

            fb value:
              parseNodes defaultLabel copyDefault: d Colon: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInBlock = ( |
            | 
            parseSwitchLabelIfFound: [|:r| ^ r].
            resend.parseInBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchBlockStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseSwitchLabelIfFound: fb = ( |
            | 
            parseDefaultIfFound: [|:r| ^ fb value: r].
            parseCaseIfFound:    [|:r| ^ fb value: r].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         switchStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser switchStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser switchStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'switchStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
             epl.
             sb.
             sw.
            | 
            sw: parseKeyword: 'switch'.

            nodesToParse isEmpty  ifTrue: [
              failNode: sw
               Because: 'No parenthesized expression found after'
            ].
            epl: parseExpressionParenList.

            nodesToParse isEmpty ifTrue: [
              failNode: epl
               Because: 'No block found in switch statement after'
            ].
            nodesToParse first isCurlyList ifFalse: [
              failAll: nodesToParse
              Because: 'Expected a block in curly braces instead of'
            ].
            sb: switchBlockParser
                   copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                              IfFail: [|:e| ^ myFailBlock value: e].

            parseNodes switchStatement 
              copySwitch: sw 
                ExpressionInParens: epl
                Block: sb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         synchronizedStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser synchronizedStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser synchronizedStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInContext = ( |
            | parseSynchronizedStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSynchronized = ( |
            | 
            parseKeyword: 'synchronized').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'synchronizedStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSynchronizedStatement = ( |
            | 
            parseNodes synchronizedStatement
                 copySynchronized: parseSynchronized
              ExpressionParenList: parseExpressionParenList
                            Block: parseBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         throwStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser throwStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser throwStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseInContext = ( |
            | parseThrowStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseThrowStatement = ( |
             e.
             s.
             throw.
            | 
            nodesToParse isEmpty ifTrue: [failBecause: 'expected `throw\' after'].
            throw: nodesToParse removeFirst.
            (throw isJavaKeyword: 'throw') ifFalse: [failNode: throw Because: 'expected `throw\' instead of'].
            errorNode: throw.
            e: parseExpression: nodesToParse UpTo: ';'.
            errorNode: e.
            s: parseSemicolon.
            parseNodes throwStatement copyThrow: throw Expression: e Semicolon: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: methods\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         throwsParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwsParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwsParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser throwsParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwsParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwsParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser throwsParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwsParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'throwsParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
             exceptionNames.
             throws.
            | 
            exceptionNames: list copyRemoveAll.

                nodesToParse isEmpty not
            && [nodesToParse first isJavaKeyword: 'throws']
             ifFalse: [
              ^ parseNodes throws copyThrowsNothing: nodesToParse
            ].
            throws: nodesToParse removeFirst.
            errorNode: throws.
            [|:exit|
              nodesToParse isEmpty ifTrue: [
                failBecause: 'missing exception name after'
              ].
              exceptionNames addLast:
                nameParser copyParseNonEmpty: nodesToParse IfFail: myFailBlock.

                  nodesToParse isEmpty not 
              && [nodesToParse first isComma]
               ifFalse: exit.

              errorNode: nodesToParse removeFirst.
            ] loopExit.

            parseNodes throws copyThrows: throws Exceptions: exceptionNames).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: statements\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         tryStatementParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser absStatementOrDclParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser tryStatementParser.

CopyDowns:
globals javaParser absStatementOrDclParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         finally.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (nil)'
        
         finallyBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser tryStatementParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         hasFinally = ( |
            | 
            nodesToParse anySatisfy: [|:n| n isJavaKeyword: 'finally']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'absStatementOrDclParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseACatch = ( |
             block.
             catch.
             parms.
            | 
            catch: nodesToParse removeFirst.
            nodesToParse isEmpty ifTrue: [
              failNode: catch
               Because: 'Expected exception parameter in parentheses after'
            ].
            nodesToParse first isParenList ifFalse: [
              failNode: nodesToParse first
               Because: 'Expected exception parameter in parentheses instead of'
            ].
            errorNode: nodesToParse first. "for parseBlock below"
            parms: formalParameterListParser
              copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                         IfFail: [|:e| ^ myFailBlock value: e].

            block: parseBlock.

            parseNodes catchClause copyCatch: catch ParameterList: parms Block: block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseCatches = ( |
             catches.
            | 
            catches: list copyRemoveAll.
            [ |:exit. catch|
              nodesToParse isEmpty ifTrue: exit.
              catch: nodesToParse first.
              (catch isJavaKeyword: 'catch')  ifFalse: exit.
              catches addLast: parseACatch.
            ] loopExit.
            catches).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseFinally = ( |
            | 
            parseKeyword: 'finally').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseInContext = ( |
            | parseTryStatement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseTry = ( |
            | 
            parseKeyword: 'try').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tryStatementParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parseTryStatement = ( |
            | 
            hasFinally ifFalse: [
              parseNodes tryStatement
                   copyTry: parseTry
                  TryBlock: parseBlock 
                   Catches: parseCatches
            ] True: [
              parseNodes tryStatement
                   copyTry: parseTry
                  TryBlock: parseBlock 
                   Catches: parseCatches
                   Finally: parseFinally
              FinallyBlock: parseBlock
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         typeParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser typeParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser typeParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
            nodesToParse isEmpty ifTrue: [
              failBecause: 'missing type after'
            ].
            parseBasicTypeIfFound: [|:r| ^ arrayify: r].
            parseReferenceType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'typeParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parseReferenceType = ( |
            | 
            arrayify: parseBasicReferenceType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fCategory: miniparsers\x7fCategory: expressions\x7fModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         variableInitializerParser = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'variableInitializerParser' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser miniParser copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'variableInitializerParser' -> () From: ( |
             {} = 'Comment: handy place for Java-specific
miniparser utilities\x7fModuleInfo: Creator: globals javaParser variableInitializerParser.

CopyDowns:
globals javaParser miniParser. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'variableInitializerParser' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'variableInitializerParser' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser variableInitializerParser parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'variableInitializerParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'miniParser' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'variableInitializerParser' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         parse = ( |
            | 
                nodesToParse isEmpty
            || [nodesToParse first isJavaSeparator]  ifTrue: [
              failBecause: 'no initialization expression found after'.
            ].
            nodesToParse first isCurlyList
             ifTrue: [ arrayInitializerParser
                         copyParseNonEmpty: nodesToParse removeFirst subnodes copy
                                    IfFail: myFailBlock
            ] False: [|e. |
              errorNode: nodesToParse first.
              e: parseExpression: nodesToParse UpTo: ';,'.
              parseNodes scalarInitializer copyExpression: e
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         javaMinis = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaMinis.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.10 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaMinis' -> () From: ( | {
         'ModuleInfo: Module: javaMinis InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaMinis postFileIn
