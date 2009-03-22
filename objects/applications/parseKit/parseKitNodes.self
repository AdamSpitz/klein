 '$Revision: 30.10 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parseKitNodes = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitNodes.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.10 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitNodes' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'parseKitTokens'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         parseNodes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: basic nodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         node = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals parseKit parseNodes node.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: InitializeToExpression: (vector)'
        
         myPostComments <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: InitializeToExpression: (vector)'
        
         myPreComments <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         mySubnodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes node parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | extent < x extent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | == x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addAllSubnodes: nodes = ( |
            | 
            mySubnodes addAll: nodes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addPostComment: n = ( |
            | 
            myPostComments: postComments copyAddLast: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addPreComment: n = ( |
            | 
            myPreComments: preComments copyAddLast: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnode: n = ( |
            | 
            mySubnodes: mySubnodes asList addLast: n. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: tree outliner for debugging\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         asDebuggingOutliner = ( |
            | 
            parseKit parseNodeOuterOutliner copyNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: tree outliner for debugging\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         asMorph = ( |
            | 
            asDebuggingOutliner).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         basicTestResultString = ( |
            | nodeTypeName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((clone
              copySubnodesFrom: self)
              myPostComments: myPostComments copy)
              myPreComments:  myPreComments copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            (clone
                removeAllSubnodes
                myPostComments: vector)
                myPreComments: vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: copying\x7fCategory: helpers\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | mySubnodes: n subnodes copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doAllTokens: blk = ( |
            | 
            preComments do: blk.
            isToken ifTrue: [blk value: self]
                     False: [ doSubnodes: [|:n| n doAllTokens: blk]].
            postComments do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doSubnodes: b = ( |
            | subnodes do: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doSubnodesAndComments: b = ( |
             r.
            | 
            preComments do: [|:n| r: b value: n].
            r: doSubnodes: b.
            postComments do: [|:n| r: b value: n].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doSubnodesWithContext: aBlock InitialResult: init = ( |
             r.
             v.
            | 
            v: subnodes asVector.
            r: init.
            v do: [|:n. :i|
              r: aBlock value: n With: self With: (v at: i pred IfAbsent: nil) With: (v at: i succ IfAbsent: nil) With: r.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | 
            hasSubnodes ifTrue: [lastSubnode end]
                         False: [parseKit inputPosition]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         extent = ( |
            | 
                 firstSubnode start
            to:   lastSubnode end).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstSubnode = ( |
            | subnodes first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstToken = ( |
            | 
            firstSubnode firstToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasComments = ( |
            | preComments isEmpty not || [postComments isEmpty not]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasSubnodes = ( |
            | subnodes isEmpty not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         in = ( |
            | 
            firstSubnode in).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBracketedList = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isCurlyList = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isParenList = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: tree outliner for debugging\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isShowableAsMorph = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isSquareList = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isToken = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lastSubnode = ( |
            | subnodes last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lastToken = ( |
            | lastSubnode lastToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fComment: Apply aBlock in preOrder. If block returns something new, that replaces
the old node in the tree.\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         mapMeAndAllMySubnodesBy: aBlock = ( |
             newNode.
            | 
            newNode: aBlock value: self.
            = newNode ifFalse: [^ newNode].
            newwSubnodes: subnodesToTransform copyMappedBy: [|:n. nn|
              nn: mapMeAndAllMySubnodesBy: aBlock.
              changed: changed || [n !== nn].
              nn
            ].
            changed ifTrue: [copy transformedSubndoes: newSubnodes] False: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         mapSubnodesBy: aBlock = ( |
             changed <- bootstrap stub -> 'globals' -> 'false' -> ().
             newSubnodes.
             r.
            | 
            newSubnodes: subnodesToTransform copyMappedBy: [|:n. nn. |
              nn: aBlock value: n With: self.
              changed: changed || [n !== nn].
              nn
            ].
            changed ifTrue: [copy transformedSubnodes: newSubnodes] False: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nodeTypeName = ( |
            | 
            asMirror safeName asTokensSeparatedByWhiteSpace last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         postComments = ( |
            | 
            myPostComments).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         postComments: aCollection = ( |
            | myPostComments: aCollection asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         postCommentsOfLastToken = ( |
            | 
            lastToken postComments).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         postOrderMapAllSubnodesBy: aBlock = ( |
             changed <- bootstrap stub -> 'globals' -> 'false' -> ().
             newSubnodes.
             r.
            | 
            newSubnodes: subnodesToTransform copyMappedBy: [|:n. nn. nnn|
              nn: n postOrderMapAllSubnodesBy: aBlock.
              nnn: aBlock value: nn With: self.
              changed: changed || [n !== nnn].
              nnn
            ].
            changed ifTrue: [copy transformedSubnodes: newSubnodes] False: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preComments = ( |
            | myPreComments).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preComments: aCollection = ( |
            | myPreComments: aCollection asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preCommentsOfFirstToken = ( |
            | firstToken preComments).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preOrderMapAllSubnodesBy: aBlock = ( |
             changed <- bootstrap stub -> 'globals' -> 'false' -> ().
             newSubnodes.
             r.
            | 
            newSubnodes: subnodesToTransform copyMappedBy: [|:n. nn. nnn|
              nn: aBlock value: n With: self.
              nnn: n preOrderMapAllSubnodesBy: aBlock.
              changed: changed || [n !== nnn].
              nnn
            ].
            changed ifTrue: [copy transformedSubnodes: newSubnodes] False: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: copying\x7fCategory: helpers\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            mySubnodes: mySubnodes copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeLastSubnode = ( |
            | 
            mySubnodes removeLast. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: manipulating comments\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         setCommentsFrom: n = ( |
            | 
            preComments: n preComments.
            postComments: n postComments).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         shortPrintString = ( |
             s.
            | 
            s: source.
            nodeTypeName, 
            (s isEmpty ifTrue: '' False: [': ', (s copyAtMostWithEllipsis: 20)])).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         source = ( |
            | 
            hasSubnodes ifTrue: [in sourceAt: extent]
                         False: '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            hasSubnodes ifTrue: [firstSubnode start]
                         False: [parseKit inputPosition]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodeAndCommentCount = ( |
            | subnodeCount + preComments size + postComments size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodeAt: i = ( |
            | subnodeAt: i IfAbsent: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodeAt: i IfAbsent: ab = ( |
            | subnodes asVector at: i IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodeCount = ( |
            | subnodes size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fComment: may be overriden\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
            | 
            mySubnodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fCategory: transform helpers\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subnodesToTransform = ( |
            | subnodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         testResultString = ( |
            | 
            (testResultString: 0), '\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         testResultString: depth = ( |
             r.
            | 
            r: '\n', ('' copySize: depth FillingWith: ' ').
            r: r, basicTestResultString.
            doSubnodesAndComments: [|:n| r: r, (n testResultString: depth + 2)].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         tokenCount = ( |
             r <- 0.
            | 
            doAllTokens: [r: r succ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         tokenSource = ( |
             r <- ''.
            | 
            doAllTokens: [|:t| r: r & t source].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fCategory: transform helpers\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         transformedSubnodes: s = ( |
            | 
            mySubnodes: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: bracketed lists\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         bracketedList = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> () From: ( |
             {} = 'Comment: I contain a list of tokens within a pair of brackets
(or parens, or BEGIN END).\x7fModuleInfo: Creator: globals parseKit parseNodes bracketedList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes bracketedList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         closingNode = ( |
            | 
            lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         closingSource = ( |
            | error: 'child should implement').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOpen: o InBrackets: i Close: cl = ( |
            | 
            ((copyRemoveAll addSubnode: o) addSubnode: i) addSubnode: cl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBracketedList = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         openingNode = ( |
            | 
            firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: basic nodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         childless = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mySubnodes' From:
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> () From: ( |
             {} = 'Comment: A parse node with no children.\x7fModuleInfo: Creator: globals parseKit parseNodes childless.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: mySubnodes parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes childless parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasSubnodes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( | {
         'Category: trasforming\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         mapSubnodesBy: aBlock = ( |
            | aBlock value: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'childless' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: bracketed lists\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         curlyList = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes bracketedList copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> () From: ( |
             {} = 'Comment: I contain a list of tokens within a pair of brackets
(or parens, or BEGIN END).\x7fModuleInfo: Creator: globals parseKit parseNodes curlyList.

CopyDowns:
globals parseKit parseNodes bracketedList. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes curlyList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         closingSource = '}'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isCurlyList = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'curlyList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: bracketed lists\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         parenList = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes bracketedList copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> () From: ( |
             {} = 'Comment: I contain a list of tokens within a pair of brackets
(or parens, or BEGIN END).\x7fModuleInfo: Creator: globals parseKit parseNodes parenList.

CopyDowns:
globals parseKit parseNodes bracketedList. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes parenList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         closingSource = ')'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isParenList = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: abstract series\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         separatorList = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> () From: ( |
             {} = 'Comment: In conjunction with my miniparser, I am used for
comma-separated lists, etc.\x7fModuleInfo: Creator: globals parseKit parseNodes separatorList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         nonseparators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes separatorList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addNonseparatorNode: n = ( |
            | 
            nonseparators addLast: n.
            resend.addSubnode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSeparatorNode: n = ( |
            | resend.addSubnode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnode: n = ( |
            | error: 'use addSeparatorNode: or addNonseparatorNode:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            nonseparators: nonseparators copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'separatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            resend.removeAllSubnodes.
            nonseparators: nonseparators copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: bracketed lists\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         squareList = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes bracketedList copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> () From: ( |
             {} = 'Comment: I contain a list of tokens within a pair of brackets
(or parens, or BEGIN END).\x7fModuleInfo: Creator: globals parseKit parseNodes squareList.

CopyDowns:
globals parseKit parseNodes bracketedList. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes squareList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         closingSource = ']'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isSquareList = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'squareList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'bracketedList' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: abstract series\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         terminatorList = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> () From: ( |
             {} = 'Comment: In conjunction with my miniparser, I am used for
semiconol-terminated lists, etc.\x7fModuleInfo: Creator: globals parseKit parseNodes terminatorList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         nonterminators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes terminatorList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addNonterminatorNode: n = ( |
            | 
            nonterminators addLast: n.
            resend.addSubnode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnode: n = ( |
            | 
            error: 'use addTerminatorNode: or addNonterminatorNode:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addTerminatorNode: n = ( |
            | resend.addSubnode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            nonterminators: nonterminators copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'terminatorList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            resend.removeAllSubnodes.
            nonterminators: nonterminators copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> () From: ( | {
         'Category: basic nodes\x7fModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         unimplemented = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'unimplemented' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'unimplemented' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals parseKit parseNodes unimplemented.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'unimplemented' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'unimplemented' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodes unimplemented parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'unimplemented' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 



 '-- Sub parts'

 bootstrap read: 'parseKitTokens' From: 'applications/parseKit'



 '-- Side effects'

 globals modules parseKitNodes postFileIn
