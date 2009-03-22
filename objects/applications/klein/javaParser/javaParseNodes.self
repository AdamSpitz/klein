 '$Revision: 30.11 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         parseNodes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: with operators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractExpression = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes abstractExpression.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes abstractExpression parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractInvocation = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes abstractInvocation.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes abstractInvocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentList = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         keyword = ( |
            | 
            firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: creators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractNew = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes abstractNew.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes abstractNew parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNew: n = ( |
            | 
            copyRemoveAll addSubnode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNew: n Type: t = ( |
            | (copyNew: n) addSubnode: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 200.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newKeyword = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         type = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: selectors\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractSelector = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes abstractSelector.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes abstractSelector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDot: d = ( |
            | 
            copyRemoveAll addSubnode: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes abstractStatement.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (vector)\x7fVisibility: public'
        
         labels <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes abstractStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addLabels: lbls = ( |
            | 
            labels: lbls asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fCategory: helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            labels: n labels copy.
            resend.copySubnodesFrom: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstSubnode = ( |
            | 
            labels at: 0 IfAbsent: [resend.firstSubnode]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasSubnodes = ( |
            | labels isEmpty not || [resend.hasSubnodes]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nonLabels = ( |
            | resend.subnodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fCategory: helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            labels: vector.
            resend.removeAllSubnodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodeCount = ( |
            | labels size + resend.subnodeCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
            | labels, resend.subnodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractType = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes abstractType.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes abstractType parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBasicType = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isClassOrInterfaceType = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         argument = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes argument.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes argument parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         commaIfPresent: pb IfAbsent: ab = ( |
            | 
            subnodeCount = 2
              ifTrue: [pb value: lastSubnode]
               False:  ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e = ( |
            | 
            copyRemoveAll addSubnode: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e Comma: c = ( |
            | 
            (copyRemoveAll
            addSubnode: e)
            addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 200.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argument' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentList = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes argumentList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes argumentList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arguments = ( |
             a.
            | 
            a: subnodes copy.
            a removeFirst.
            a removeLast.
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         closeParen = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOpen: op Arguments: a Close: cl = ( |
            | 
            ((copyRemoveAll
            addSubnode: op)
            addAllSubnodes: a)
            addSubnode: cl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 190.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         openParen = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'argumentList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arrayAccess = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes arrayAccess.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes arrayAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n DimExpr: de = ( |
            | 
            (copyRemoveAll
            addSubnode: n)
            addSubnode: de).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dimensionExpression = ( |
            | 
            lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 190.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arrayClass = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes arrayClass.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes arrayClass parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n DEs: des = ( |
            | 
            (copyRemoveAll
            addSubnode: n)
            addAllSubnodes: des).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dimensionExpressions = ( |
             des.
            | 
            des: subnodes copy.
            des removeFirst.
            des).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayClass' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         variableInitializer = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes variableInitializer.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes variableInitializer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaInitializer = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arrayInitializer = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayInitializer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes variableInitializer copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayInitializer' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes arrayInitializer.

CopyDowns:
globals javaParser parseNodes variableInitializer. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayInitializer' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayInitializer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes arrayInitializer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayInitializer' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaArrayInitializer = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayInitializer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arrayType = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractType copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes arrayType.

CopyDowns:
globals javaParser parseNodes abstractType. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes arrayType parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         baseType = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBaseType: base Squares: squares = ( |
            | 
            (copyRemoveAll addSubnode: base) addAllSubnodes: squares).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rank = ( |
            | subnodeCount pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'arrayType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         squares = ( |
            | subnodes asVector copyWithoutFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         attributeName = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes attributeName.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes attributeName parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyID: theName = ( |
            | 
            copyRemoveAll addSubnode: theName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameID = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'attributeName' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         basicType = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractType copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes basicType.

CopyDowns:
globals javaParser parseNodes abstractType. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes basicType parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyKeyword: typeKeywordToken = ( |
            | 
            copyRemoveAll addSubnode: typeKeywordToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBasicType = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         type = ( |
            | typeKeywordToken source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'basicType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         typeKeywordToken = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         unorderedStatementBody = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mySubnodes' From:
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes unorderedStatementBody.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: mySubnodes parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit parseNodes node)'
        
         closeCurly <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit parseNodes node)'
        
         openCurly <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes unorderedStatementBody parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addAllSubnodes: nodes = ( |
            | 
            statementNodes addAll: nodes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnode: n = ( |
            | statementNodes addLast: n. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOpenCurly: o StatementNodes: sns CloseCurly: c = ( |
            | 
            ((copy openCurly: o) statementNodes: sns) closeCurly: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
                 openCurly: n openCurly copy.
                closeCurly: n closeCurly copy.
            statementNodes: n statementNodes copy.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstSubnode = ( |
            | openCurly).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasSubnodes = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lastSubnode = ( |
            | closeCurly).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            openCurly: parseKit parseNodes node.
            closeCurly: parseKit parseNodes node.
            statementNodes: statementNodes copyRemoveAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodeCount = ( |
            | 2 + statementNodes size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
            | 
            (statementNodes copy asList
              addFirst:   openCurly)
              addLast:   closeCurly).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: transforming\x7fCategory: transform helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subnodesToTransform = ( |
            | 
            (statementNodes copyAddFirst: openCurly) asList addLast: closeCurly).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subtreeWithoutCurlies = ( |
            | 
            parseKit parseNodes node copy addAllSubnodes: statementNodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> () From: ( | {
         'Category: transforming\x7fCategory: transform helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         transformedSubnodes: s = ( |
            | 
            statementNodes: s copy.
            openCurly: s removeFirst.
            closeCurly: s removeLast).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         statementNodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         block = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes unorderedStatementBody copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes block.

CopyDowns:
globals javaParser parseNodes unorderedStatementBody. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes block parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaBlock = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes blockStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes blockStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         block = ( |
            | nonLabels first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBlock: blkNode = ( |
            | 
            copyRemoveAll addSubnode: blkNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'blockStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         breakOrContinueStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes breakOrContinueStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes breakOrContinueStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copyKeyword: kw Semicolon: s = ( |
            | 
            (copyRemoveAll
              addSubnode: kw)
              addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copyKeyword: kw Target: id Semicolon: s = ( |
            | 
            ((copyRemoveAll
              addSubnode: kw)
              addSubnode: id)
              addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         keyword = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         targetIfPresent: pb IfAbsent: ab = ( |
            | 
            nonLabels size > 2
              ifTrue: [pb value: nonLabels asVector at: 1]
               False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         breakStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes breakOrContinueStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes breakStatement.

CopyDowns:
globals javaParser parseNodes breakOrContinueStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes breakStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         break = ( |
            | keyword).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         switchLabel = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes switchLabel.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes switchLabel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         caseLabel = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes switchLabel copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes caseLabel.

CopyDowns:
globals javaParser parseNodes switchLabel. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes caseLabel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         case = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         colon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCase: ca Expression: e Colon: c = ( |
            | 
            ((copyRemoveAll
            addSubnode: ca)
            addSubnode: e)
            addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'caseLabel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         catchClause = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes catchClause.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes catchClause parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         block = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         catch = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         copyCatch: catchKeyword ParameterList: formalParmList Block: blk = ( |
            | 
            ((copyRemoveAll
            addSubnode: catchKeyword)
            addSubnode: formalParmList)
            addSubnode: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parameterList = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'catchClause' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         interfaceBody = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceBody' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes unorderedStatementBody copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceBody' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes interfaceBody.

CopyDowns:
globals javaParser parseNodes unorderedStatementBody. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceBody' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceBody' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes interfaceBody parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceBody' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classBody = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classBody' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes interfaceBody copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classBody' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes classBody.

CopyDowns:
globals javaParser parseNodes interfaceBody. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classBody' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classBody' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes classBody parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classBody' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'unorderedStatementBody' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         startsWithModifiers = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mySubnodes' From:
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes startsWithModifiers.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: mySubnodes parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit parseNodes node)\x7fVisibility: public'
        
         modifiers <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes startsWithModifiers parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addAllSubnodes: nodes = ( |
            | 
            error: 'does not make sense for this node'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnode: n = ( |
            | 
            error: 'does not make sense for this node'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addSubnodesAfterModifiersTo: aList = ( |
            | 
            "childMustImplement"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            modifiers: n modifiers copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstSubnode = ( |
            | 
            hasModifiers
              ifTrue: [         modifiers]
               False: [nodeAfterModifiers]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasModifiers = ( |
            | nil != modifiers).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeAfterModifiers = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            modifiers: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
             r.
            | 
            r: list copyRemoveAll.
            hasModifiers ifTrue: [ r addLast: modifiers ].
            addSubnodesAfterModifiersTo: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: transforming\x7fCategory: transform helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subnodesToTransform = ( |
            | 
            declarators).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> () From: ( | {
         'Category: transforming\x7fCategory: transform helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         transformedSubnodes: s = ( |
            | 
            declarators: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         classOrInterfaceDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes startsWithModifiers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes classOrInterfaceDcl.

CopyDowns:
globals javaParser parseNodes startsWithModifiers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit parseNodes node)\x7fVisibility: public'
        
         body <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit parseNodes node)\x7fVisibility: public'
        
         classOrInterfaceToken <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit parseNodes node)\x7fVisibility: public'
        
         nameToken <- bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes classOrInterfaceDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnodesAfterModifiersTo: r = ( |
            | 
            r addLast: classOrInterfaceToken.
            r addLast: nameToken.
            r addAll: classOrInterfaceSubnodes.
            r addLast: body.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterface = ( |
            | error: 'child should return class or interface').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         classOrInterfaceSubnodes = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            classOrInterfaceToken: n classOrInterfaceToken copy.
            nameToken: n nameToken copy.
            body: n body copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isInterface = ( |
            | isClass not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lastSubnode = ( |
            | body).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeAfterModifiers = ( |
            | classOrInterfaceToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
             n.
            | 
            resend.removeAllSubnodes.
            n: parseKit parseNodes node copyRemoveAll.
            classOrInterfaceToken: n.
            nameToken: n.
            body: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> () From: ( | {
         'Category: outliner support\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsForOutliner = ( |
             r.
            | 
            r: list copyRemoveAll.
            body statementNodes do: [|:sn|
              "A var decl statement has several slots"
              r addAll: sn slotsForOutlinerInClassOrInterface: self.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes classOrInterfaceDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes classDcl.

CopyDowns:
globals javaParser parseNodes classOrInterfaceDcl. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         extendsClass = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes extendsClass.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes extendsClass)\x7fVisibility: public'
        
         extendsClassNode <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         implementsInterfaces = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'implementsInterfaces' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'implementsInterfaces' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes implementsInterfaces.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes implementsInterfaces)\x7fVisibility: public'
        
         implementsInterfacesNode <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'implementsInterfaces' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes classDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         category = 'classes'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterface = 'class'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         classOrInterfaceSubnodes = ( |
             r.
            | 
            r: list copyRemoveAll.
            r addLast: extendsClassNode.
            r addLast: implementsInterfacesNode.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classToken = ( |
            | classOrInterfaceToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            extendsClassNode: n extendsClassNode copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isClass = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | nameToken source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            resend.removeAllSubnodes.
            extendsClassNode: extendsClassNode copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes nameNode.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (vector)'
        
         names <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes nameNode parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForName: anIdentifierToken = ( |
            | 
            copyRemoveAll addSubnode: anIdentifierToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            names: n names copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 250.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | 
            names reduceWith: [|:a. :b| a, '.', b]
                 IfSingleton: [|:a| a]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            resend.removeAllSubnodes.
            names: names copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterfaceName = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceName' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes nameNode copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceName' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes classOrInterfaceName.

CopyDowns:
globals javaParser parseNodes nameNode. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceName' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceName' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes classOrInterfaceName parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceName' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'nameNode' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterfaceType = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractType copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes classOrInterfaceType.

CopyDowns:
globals javaParser parseNodes abstractType. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes classOrInterfaceType parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: nameToken = ( |
            | 
            copyRemoveAll addSubnode: nameToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isClassOrInterfaceType = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameToken = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         coersion = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes coersion.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes coersion parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         close = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOpen: op Expression: e Close: cl = ( |
            | 
            copyOpen: op TypeOrExpression: e Close: cl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOpen: op Type: t Close: cl = ( |
            | 
            copyOpen: op TypeOrExpression: t Close: cl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copyOpen: op TypeOrExpression: e Close: cl = ( |
            | ((copyRemoveAll
            addSubnode: op)
            addSubnode: e)
            addSubnode: cl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         open = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'coersion' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         typeOrExpression = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         compilationUnit = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mySubnodes' From:
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> () From: ( |
             {} = 'Comment: I represent a java class file in the AST.\x7fModuleInfo: Creator: globals javaParser parseNodes compilationUnit.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: mySubnodes parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         importDcls <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)'
        
         packageDcl.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes compilationUnit parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: manipulating subnodes\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addSubnode: n = ( |
            | 
            error: 'makes no sense').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterfaceNamed: n IfAbsent: ab = ( |
            | 
            typeDcls findFirst: [|:t| t name = n] IfPresent: [|:td| td] IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            packageDcl: n packageDcl copy.
            importDcls: n importDcls copy.
              typeDcls: n typeDcls copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doSubnodes: b = ( |
             r.
            | 
            hasPackageDcl ifTrue: [r: b value: packageDcl].
            importDcls do: [|:n| r: b value: n].
            typeDcls do: [|:n| r: b value: n].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstSubnode = ( |
            | 
            doSubnodes: [|:n| ^ n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasPackageDcl = ( |
            | nil != packageDcl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         haveImportDcls = ( |
            | importDcls isEmpty not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         haveTypeDcls = ( |
            | typeDcls isEmpty not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lastSubnode = ( |
            | 
            case if: haveTypeDcls Then: [typeDcls last]
                 If: [haveImportDcls] Then: [importDcls last]
                 If: [hasPackageDcl] Then: [packageDcl]
                 Else: [error: 'empty']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         packageName = ( |
             r <- ''.
            | 
            nil = packageDcl ifTrue: [^''].
            packageDcl nameNode name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            packageDcl: nil.
            importDcls: list copyRemoveAll.
            typeDcls: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: UI2 support\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsForOutliner = ( |
             r.
            | 
            r: list copyRemoveAll.
            r addAll: importDcls copyMappedBy: [|:d| javaParser importDcl        copyParseTree: d ].
            r addAll:   typeDcls copyMappedBy: [|:d| javaParser classOrInterface copyParseTree: d ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
             r.
            | 
            r: list copyRemoveAll.
            hasPackageDcl ifTrue: [r addLast: packageDcl].
            r addAll: importDcls.
            r addAll: typeDcls.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         typeDcls <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: formal parameters\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         formalParameterList = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes formalParameterList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes formalParameterList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyLeft: l Formals: f Right: r = ( |
            | 
            ((copyRemoveAll addSubnode: l) addAllSubnodes: f) addSubnode: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         formals = ( |
             s.
            | 
            s: subnodes asList copy.
            s removeFirst.
            s removeLast.
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         leftParen = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rightParen = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         methodOrConstructorDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes startsWithModifiers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes methodOrConstructorDcl.

CopyDowns:
globals javaParser parseNodes startsWithModifiers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes block)'
        
         body <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes formalParameterList)'
        
         formalParameterList <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameterList' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser lexer tokens identifier)'
        
         nameID <- bootstrap stub -> 'globals' -> 'javaParser' -> 'lexer' -> 'tokens' -> 'identifier' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes methodOrConstructorDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addSubnodesAfterModifiersTo: aList = ( |
            | 
            aList addAll: (type & nameID & formalParameterList & throws & body) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            type: n type copy.
            nameID: n nameID copy.
            throws: n throws copy.
            formalParameterList: n formalParameterList copy.
            body: n body copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'Category: outliner support\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | nameID source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeAfterModifiers = ( |
            | type).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
             n.
            | 
            resend.removeAllSubnodes.
            n: parseKit parseNodes node copyRemoveAll.
            type: n.
            throws: n.
            nameID: n.
            formalParameterList: n.
            body: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> () From: ( | {
         'Category: outliner support\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsForOutlinerInClassOrInterface: holder = ( |
            | 
            vector copyAddLast:
              javaParser methodOrConstructorSlot
                 copyClassOrInterface: holder
                MethodOrConstructorDcl: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         throws = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes throws.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes throws)'
        
         throws <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes abstractType)'
        
         type <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit inputExtent)'
        
         extent <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (parseKit inputStream)'
        
         in <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes throws parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyThrows: t Exceptions: names = ( |
             r.
            | 
            r: copyRemoveAll.
            r in: t in.
            r extent: t start to: names last end.
            r addSubnode: t.
            r addAllSubnodes: names).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyThrowsNothing: nodes = ( |
             r.
            | 
            r: copyRemoveAll.
            nodes isEmpty ifFalse: [
              r in: nodes first in.
              r extent: nodes first start to: nodes first start.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         exceptionNames = ( |
             r.
            | 
            r: subnodes asList copy.
            r removeFirst.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeAfterModifiers = ( |
            | type).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throws' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         throwsIdentifier = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         constructorDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes methodOrConstructorDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes constructorDcl.

CopyDowns:
globals javaParser parseNodes methodOrConstructorDcl. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes constructorDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFrom: aMethodDcl = ( |
            | 
            ((copyRemoveAll
              copySubnodesFrom: aMethodDcl)
              myPreComments: aMethodDcl myPreComments copy)
              myPostComments: aMethodDcl myPostComments copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaConstructor = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         methodOrConstructor = 'constructor'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'constructorDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         continueStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'continueStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes breakOrContinueStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'continueStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes continueStatement.

CopyDowns:
globals javaParser parseNodes breakOrContinueStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'continueStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'continueStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes continueStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'continueStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         continue = ( |
            | keyword).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'continueStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'breakOrContinueStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultLabel = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes switchLabel copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes defaultLabel.

CopyDowns:
globals javaParser parseNodes switchLabel. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes defaultLabel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         colon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDefault: d Colon: c = ( |
            | 
            (copyRemoveAll
            addSubnode: d)
            addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         default = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'defaultLabel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchLabel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: creators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dimensionExpression = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes dimensionExpression.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes dimensionExpression parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyLeft: lft Expression: e Right: r = ( |
            | 
            ((copyRemoveAll
            addSubnode: lft)
            addSubnode: e)
            addSubnode: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyLeft: lft Right: r = ( |
            | 
            (copyRemoveAll
            addSubnode: lft)
            addSubnode: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionIfPresent: pb IfAbsent: ab = ( |
            | 
            subnodeCount > 2
             ifTrue: [ pb value: subnodeAt: 1 ]
              False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasExpression = ( |
            | 
            expressionIfPresent: true IfAbsent: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         left = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dimensionExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         right = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes doStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes doStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDo: d Statement: s While: w ExpressionInParens: e Semicolon: semi = ( |
            | 
            ((((
            copyRemoveAll
            addSubnode: d)
            addSubnode: s)
            addSubnode: w)
            addSubnode: e)
            addSubnode: semi).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         do = ( |
            | 
            firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionInParens = ( |
            | 
            nonLabels asVector at: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | 
            lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statement = ( |
            | 
            nonLabels asVector at: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'doStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         while = ( |
            | 
            nonLabels asVector at: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dotClassType = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractType copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes dotClassType.

CopyDowns:
globals javaParser parseNodes abstractType. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes dotClassType parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         class = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyPreDot: p Dot: d Class: c = ( |
            | 
            ((copyRemoveAll
            addSubnode: p)
            addSubnode: d)
            addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isClassOrInterfaceType = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractType' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'dotClassType' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preDot = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         emptyStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes emptyStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes emptyStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySemicolon: s = ( |
            | 
            copyRemoveAll addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'emptyStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: with operators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionIf = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractExpression copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes expressionIf.

CopyDowns:
globals javaParser parseNodes abstractExpression. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes expressionIf parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         colon = ( |
            | 
            subnodeAt: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyE1: e1 Question: q E2: e2 Colon: c E3: e3 = ( |
            | 
            ((((copyRemoveAll
            addSubnode: e1)
            addSubnode: q)
            addSubnode: e2)
            addSubnode: c)
            addSubnode: e3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression1 = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression2 = ( |
            | 
            subnodeAt: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression3 = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 90.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesForEqualPrecedenceSubnode: n = ( |
            | 
            n !== expression3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionIf' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         questionMark = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes expressionStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes expressionStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e Semicolon: s = ( |
            | 
            (copyRemoveAll
            addSubnode: e)
            addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 20.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'expressionStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes extendsClass parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classNode = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         extendsNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsClass' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         extendsInterfaces = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes extendsInterfaces.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes extendsInterfaces parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         extendsNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         interfaceNodes = ( |
             n.
             r.
            | 
            r: list copyRemoveAll.
            n: subnodes copy asList.
            [n isEmpty] whileFalse: [n removeFirst. r addLast: n removeFirst].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: selectors\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         fieldSelector = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractSelector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes fieldSelector.

CopyDowns:
globals javaParser parseNodes abstractSelector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes fieldSelector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDot: d Name: id = ( |
            | 
            (copyDot: d) addSubnode: id).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: for parts\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forCond = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes forCond.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes forCond parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e Semicolon: s = ( |
            | 
            (copyRemoveAll
            addSubnode: e)
            addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySemicolon: s = ( |
            | 
            copyRemoveAll addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionIfPresent: pb IfAbsent: ab = ( |
            | 
            isExplicit ifTrue: [pb value: firstSubnode]
                        False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isExplicit = ( |
            | subnodeCount > 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forCond' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: for parts\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forInit = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mySubnodes' From:
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes forInit.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: mySubnodes parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isExplicit <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isLocalVariableDcl <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         isStatementExpressionList.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myExpressionList.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myLocalVariableDclStmt.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         mySemicolon.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes forInit parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpressionList: el Semicolon: s = ( |
            | 
            (((copyRemoveAll
            isExplicit: true)
            isLocalVariableDcl: false)
            myExpressionList: el)
            mySemicolon: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyLocalVarDclStmt: s = ( |
            | 
            ((copyRemoveAll
            isExplicit: true)
            isLocalVariableDcl: true)
            myLocalVariableDclStmt: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySemicolon: s = ( |
            | 
            ((copyRemoveAll
            isExplicit: false)
            isLocalVariableDcl: false)
            mySemicolon: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fCategory: helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            isExplicit:                  n isExplicit.
            isLocalVariableDcl:          n isLocalVariableDcl.
            isStatementExpresssionList:  n isStatementExpressionList.
            myExpressionList:            n myExpressionList.
            myLocalVariableDclStmt:      n myLocalVariableDclStmt.
            mySemicolon:                 n mySemicolon).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: copying\x7fCategory: helpers\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            isExplicit: false.
            isLocalVariableDcl: false.
            isStatementExpressionList: false.
            myExpressionList: nil.
            myLocalVariableDclStmt: nil.
            mySemicolon: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forInit' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
            | 
            case if:   [ isExplicit not     ] Then: [ list copyRemoveAll addLast: mySemicolon ]
                 If:   [ isLocalVariableDcl ] Then: [ list copyRemoveAll addLast: myLocalVariableDclStmt ]
                                              Else: [ (myExpressionList & mySemicolon) asList ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: for parts\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forParenList = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes forParenList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes forParenList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         close = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         cond = ( |
            | 
            subnodeAt: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOpen: o Init: i Cond: c UpdateOrNil: u Close: cl = ( |
            | 
            copyRemoveAll addAllSubnodes:
              nil = u  ifTrue: [(o & i & c     & cl) asVector]
                        False: [(o & i & c & u & cl) asVector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasUpdate = ( |
            | subnodeCount > 4).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         init = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         open = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forParenList' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         updateIfPresent: pb IfAbsent: ab = ( |
            | 
            hasUpdate ifTrue: [pb value: subnodeAt: 3]
                       False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes forStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes forStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFor: f ParenList: fpl Statement: s = ( |
            | 
            ((copyRemoveAll
            addSubnode: f)
            addSubnode: fpl)
            addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         for = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forParenList = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statement = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: for parts\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forUpdate = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes forUpdate.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes forUpdate parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpressionList: el = ( |
            | 
            copyRemoveAll addSubnode: el).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionList = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'forUpdate' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: formal parameters\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         formalParameter = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes formalParameter.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes formalParameter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         comma = ( |
            | 
            lastSubnode isJavaSeparator ifTrue: [lastSubnode] False: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         comma: c = ( |
            | addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyType: t Name: n Squares: aList = ( |
            | 
            ((copyRemoveAll
             addSubnode: t)
             addSubnode: n)
             addAllSubnodes: aList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         squares = ( |
             s.
            | 
            s: subnodes copy.
            s removeFirst. "type"
            s removeFirst. "name"
            s last isComma ifTrue: [s removeLast].
            s asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         type = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         ifStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes ifStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes ifStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         conditionInParens = ( |
            | 
            nonLabels asVector at: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyIf: if ConditionInParens: c Then: stmt = ( |
            | 
            ((copyRemoveAll
            addSubnode: if)
            addSubnode: c)
            addSubnode: stmt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyIf: if ConditionInParens: c Then: stmt ElseKeyword: elseKW ElseStatement: estmt = ( |
            | 
            ((copyIf: if ConditionInParens: c Then: stmt)
            addSubnode: elseKW)
            addSubnode: estmt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         elseKeywordAndStatementIfPresent: pb IfAbsent: ab = ( |
            | 
            hasElse 
              ifTrue: [pb value: (nonLabels asVector at: 3) With: nonLabels asVector at: 4]
               False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasElse = ( |
            | nonLabels size > 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         if = ( |
            | 
            nonLabels first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'ifStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         thenStatement = ( |
            | 
            nonLabels asVector at: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'implementsInterfaces' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'implementsInterfaces' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes implementsInterfaces parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'implementsInterfaces' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         importOrPackageDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes importOrPackageDcl.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)'
        
         myNameNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes importOrPackageDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyKeyword: kw Name: n Semi: s = ( |
            | 
            (copyRemoveAll myNameNode: n) addAllSubnodes: (kw & n & s) asList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         keywordNode = ( |
            | subnodes first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | nameNode source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | myNameNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semiNode = ( |
            | subnodes last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         importDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes importOrPackageDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes importDcl.

CopyDowns:
globals javaParser parseNodes importOrPackageDcl. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes importDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         keyword = 'import'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsForOutliner = ( |
            | 
            vector copyAddFirst: 
              javaParser importDcl copyParseTree: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: with operators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         infixExpression = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractExpression copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes infixExpression.

CopyDowns:
globals javaParser parseNodes abstractExpression. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes infixExpression parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         associatesLToR = ( |
            | 
            associatesRToL not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         associatesRToL = ( |
            | 
            rToLOperators includes: operator source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e1 Operator: op Expression: e2 = ( |
            | 
            ((copyRemoveAll
            addSubnode: e1)
            addSubnode: op)
            addSubnode: e2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression1 = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression2 = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         infixOpPrecedence <- bootstrap setObjectAnnotationOf: ( [|d|
	d: dictionary copyRemoveAll.
	d at: ('!=') Put: (0.5).
	d at: ('%') Put: (0.9).
	d at: ('&&') Put: (0.19).
	d at: ('&') Put: (0.39999998).
	d at: ('*') Put: (0.9).
	d at: ('+') Put: (0.8).
	d at: ('-') Put: (0.8).
	d at: ('/') Put: (0.9).
	d at: ('<') Put: (0.6).
	d at: ('<<') Put: (0.7).
	d at: ('<=') Put: (0.6).
	d at: ('==') Put: (0.5).
	d at: ('>') Put: (0.6).
	d at: ('>=') Put: (0.6).
	d at: ('>>') Put: (0.7).
	d at: ('>>>') Put: (0.7).
	d at: ('^') Put: (0.3).
	d at: ('instanceof') Put: (0.6).
	d at: ('|') Put: (0.19999999).
	d at: ('||') Put: (0.18).
] value) From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes infixExpression parent infixOpPrecedence.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = ( |
            | 
            [todo cleanup]. "rename to precedence if all java nodes can be child of something"
            100 + precedenceOfMyOperator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesForEqualPrecedenceSubnode: n = ( |
            | 
            (n == expression1) = associatesRToL).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         operator = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         precedenceOfMyOperator = ( |
            | 
            infixOpPrecedence at: operator source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'infixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rToLOperators = bootstrap setObjectAnnotationOf: ( (('%=')
	& ('&=')
	& ('*=')
	& ('+=')
	& ('-=')
	& ('/=')
	& ('<<')
	& ('<<=')
	& ('=')
	& ('>>')
	& ('>>=')
	& ('>>>')
	& ('>>>=')
	& ('^=')
	& ('|=')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes infixExpression parent rToLOperators.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         interfaceDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes classOrInterfaceDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes interfaceDcl.

CopyDowns:
globals javaParser parseNodes classOrInterfaceDcl. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (javaParser parseNodes extendsInterfaces)\x7fVisibility: public'
        
         extendsInterfacesNode <- bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'extendsInterfaces' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes interfaceDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterface = 'interface'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         classOrInterfaceSubnodes = ( |
             r.
            | 
            r: list copyRemoveAll.
            r addLast: extendsInterfacesNode.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            extendsInterfacesNode: n extendsInterfacesNode copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         interfaceToken = ( |
            | classOrInterfaceToken).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isClass = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'classOrInterfaceDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'interfaceDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            resend.removeAllSubnodes.
            extendsInterfacesNode: extendsInterfacesNode copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         label = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes label.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes label parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         colon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyIdentifier: id Colon: c = ( |
            | 
            (copyRemoveAll
            addSubnode: id)
            addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         identifer = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: selectors\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         messageSelector = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes fieldSelector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes messageSelector.

CopyDowns:
globals javaParser parseNodes fieldSelector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes messageSelector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arguments = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDot: d Name: n Arguments: a = ( |
            | 
            (copyDot: d Name: n) addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 180.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'messageSelector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'fieldSelector' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         methodDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes methodOrConstructorDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes methodDcl.

CopyDowns:
globals javaParser parseNodes methodOrConstructorDcl. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes methodDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyModifiers: mods Type: typeNode Name: ident Formals: f Throws: t Body: b = ( |
            | 
            (((((copyRemoveAll
                         modifiers: mods)
                              type: typeNode)
                            nameID: ident)
               formalParameterList: f)
                            throws: t)
                              body: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         methodOrConstructor = 'method'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodOrConstructorDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         methodInvocation = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractInvocation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes methodInvocation.

CopyDowns:
globals javaParser parseNodes abstractInvocation. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes methodInvocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arguments = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Arguments: a = ( |
            | 
            (copyRemoveAll
            addSubnode: n)
            addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> 'parent' -> () From: ( | {
         'Category: semanticcs\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 180.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'methodInvocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         modifiers = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'modifiers' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'modifiers' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes modifiers.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'modifiers' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'modifiers' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes modifiers parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'modifiers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         superInvocation = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractInvocation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes superInvocation.

CopyDowns:
globals javaParser parseNodes abstractInvocation. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes superInvocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySuper: s Arguments: a = ( |
            | 
            (copyRemoveAll
            addSubnode: s)
            addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySuper: s Field: id Arguments: a = ( |
            | 
            ((copyRemoveAll
            addSubnode: s)
            addSubnode: id)
            addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         fieldIfPresent: pb IfAbsent: ab = ( |
            | 
            subnodeCount > 2
              ifTrue: [ pb value: subnodeAt: 1 ]
               False:   ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         namedSuperInvocation = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes superInvocation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes namedSuperInvocation.

CopyDowns:
globals javaParser parseNodes superInvocation. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes namedSuperInvocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Dot: d Super: s Arguments: a = ( |
            | 
            (((copyRemoveAll
            addSubnode: n)
            addSubnode: d)
            addSubnode: s)
            addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         keyword = ( |
            | 
            subnodeAt: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'namedSuperInvocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superInvocation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: creators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newArray = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractNew copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes newArray.

CopyDowns:
globals javaParser parseNodes abstractNew. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes newArray parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arrayInitializerIfPresent: pb IfAbsent: ab = ( |
             ai.
            | 
            ai: lastSubnode.
            ai isJavaArrayInitializer ifTrue: [pb value: ai]
                                       False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNew: n Type: t DimensionExpressions: des = ( |
            | 
            (copyNew: n Type: t) addAllSubnodes: des).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNew: n Type: t DimensionExpressions: des ArrayInitializer: ai = ( |
            | 
            (copyNew: n Type: t DimensionExpressions: des) addSubnode: ai).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dimensionExpressions = ( |
             r.
            | 
            r: subnodes copy asList.
            r removeFirst.
            r removeFirst.
            r last isJavaArrayInitializer ifTrue: [r removeLast].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newArray' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: creators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newInnerCreator = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes newInnerCreator.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes newInnerCreator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Dot: d NewInstance: i = ( |
            | 
            ((copyRemoveAll
            addSubnode: n)
            addSubnode: d)
            addSubnode: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 200.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nameNode = ( |
            | firstSubnodeNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newInstance = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: selectors\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newInnerCreatorSelector = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractSelector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes newInnerCreatorSelector.

CopyDowns:
globals javaParser parseNodes abstractSelector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes newInnerCreatorSelector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDot: d Creator: c = ( |
            | 
            (copyRemoveAll
            addSubnode: d)
            addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         creator = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInnerCreatorSelector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: creators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newInstance = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractNew copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes newInstance.

CopyDowns:
globals javaParser parseNodes abstractNew. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes newInstance parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentListIfPresent: pb IfAbsent: ab = ( |
            | 
            subnodeCount  >= 3
             ifTrue: [ pb value: subnodeAt: 2 ]
              False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNew: n Type: t Arguments: argList = ( |
            | 
            (copyNew: n Type: t) addSubnode: argList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractNew' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: creators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         newInstanceOfAnon = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes newInstance copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes newInstanceOfAnon.

CopyDowns:
globals javaParser parseNodes newInstance. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes newInstanceOfAnon parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         classBody = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNew: n Type: t Arguments: a ClassBody: cb = ( |
            | 
            (copyNew: n Type: t Arguments: a) addSubnode: cb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstanceOfAnon' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'newInstance' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         packageDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'packageDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes importOrPackageDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'packageDcl' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes packageDcl.

CopyDowns:
globals javaParser parseNodes importOrPackageDcl. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'packageDcl' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'packageDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes packageDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'packageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         keyword = 'package'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'packageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'importOrPackageDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: with operators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         postfixExpression = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractExpression copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes postfixExpression.

CopyDowns:
globals javaParser parseNodes abstractExpression. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes postfixExpression parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e Operator: o = ( |
            | 
            (copyRemoveAll
            addSubnode: e)
            addSubnode: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 170.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesForEqualPrecedenceSubnode: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         operator = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'postfixExpression' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: with operators\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         prefixExpression = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractExpression copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes prefixExpression.

CopyDowns:
globals javaParser parseNodes abstractExpression. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes prefixExpression parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyOperator: op Expression: e = ( |
            | 
            (copyRemoveAll
            addSubnode: op)
            addSubnode: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 180.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesForEqualPrecedenceSubnode: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         operator = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'prefixExpression' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractExpression' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: selectors\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         qualifiedSuperSelector = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractSelector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes qualifiedSuperSelector.

CopyDowns:
globals javaParser parseNodes abstractSelector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes qualifiedSuperSelector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDot: d1 Super: s Dot: d2 Name: id = ( |
            | 
            (((copyRemoveAll
            addSubnode: d1)
            addSubnode: s)
            addSubnode: d2)
            addSubnode: id).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot1 = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot2 = ( |
            | 
            subnodeAt: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | 
            subnodeAt: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractSelector' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         super = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fCategory: selectors\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         qualifiedSuperInvocationSelector = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes qualifiedSuperSelector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes qualifiedSuperInvocationSelector.

CopyDowns:
globals javaParser parseNodes qualifiedSuperSelector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes qualifiedSuperInvocationSelector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arguments = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDot: d1 Super: s Dot: d2 Name: n Arguments: a = ( |
            | 
            (copyDot: d1 Super: s Dot: d2 Name: n)
            addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperInvocationSelector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'qualifiedSuperSelector' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         returnStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes returnStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes returnStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyReturn: r Expression: e Semicolon: s = ( |
            | 
            ((copyRemoveAll
              addSubnode: r)
              addSubnode: e)
              addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyReturn: r Semicolon: s = ( |
            | 
            (copyRemoveAll 
            addSubnode: r)
            addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionIfPresent: pb IfAbsent: ab = ( |
            | 
            nonLabels size > 2
              ifTrue: [ pb value: nonLabels asVector at: 1 ]
               False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         return = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'returnStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         scalarInitializer = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes variableInitializer copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes scalarInitializer.

CopyDowns:
globals javaParser parseNodes variableInitializer. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes scalarInitializer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpression: e = ( |
            | copyRemoveAll addSubnode: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> 'parent' -> () From: ( | {
         'Category: semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 100.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'scalarInitializer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableInitializer' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         specialIDPrimary = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes specialIDPrimary.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes specialIDPrimary parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Dot: d KW: k = ( |
            | 
            (((copyRemoveAll)
            addSubnode: n)
            addSubnode: d)
            addSubnode: k).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dot = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         keyword = ( |
            | 
            subnodeAt: 2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'specialIDPrimary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: for parts\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statementExpressionList = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes statementExpressionList.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (vector)\x7fVisibility: public'
        
         expressions <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes statementExpressionList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyExpressions: exprs Nodes: nodes = ( |
            | 
            (copyRemoveAll
            addAllSubnodes: nodes)
            expressions: exprs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'statementExpressionList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: classes and interfaces\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         staticInitializer = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes staticInitializer.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes staticInitializer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         block = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyStatic: s Block: b = ( |
            | 
            (copyRemoveAll
             addSubnode: s)
             addSubnode: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         slotsForOutlinerInClassOrInterface: huh = ( |
            | 
            [todo maybe unimplemented]. vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'staticInitializer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         static = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         superFieldAccess = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes superFieldAccess.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes superFieldAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySuper: s Field: id = ( |
            | 
            (copyRemoveAll
            addSubnode: s)
            addSubnode: id).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         field = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'superFieldAccess' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         super = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         switchBlock = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchBlock' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes block copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchBlock' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes switchBlock.

CopyDowns:
globals javaParser parseNodes block. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchBlock' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchBlock' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes switchBlock parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'block' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         switchStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes switchStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes switchStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         block = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySwitch: s ExpressionInParens: epl Block: b = ( |
            | 
            ((copyRemoveAll
            addSubnode: s)
            addSubnode: epl)
            addSubnode: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         switch = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'switchStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         switchExpressionInParens = ( |
            | 
            nonLabels asVector at: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         synchronizedStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes synchronizedStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes synchronizedStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         block = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copySynchronized: s ExpressionParenList: epl Block: b = ( |
            | 
            ((copyRemoveAll
            addSubnode: s)
            addSubnode: epl)
            addSubnode: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionParenList = ( |
            | 
            nonLabels asVector at: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'synchronizedStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         synchronized = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: expressions\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         thisInvocation = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'thisInvocation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractInvocation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'thisInvocation' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes thisInvocation.

CopyDowns:
globals javaParser parseNodes abstractInvocation. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'thisInvocation' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'thisInvocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes thisInvocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'thisInvocation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyThis: t Arguments: a = ( |
            | 
            (copyRemoveAll
            addSubnode: t)
            addSubnode: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'thisInvocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractInvocation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         throwStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes throwStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes throwStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyThrow: t Expression: e Semicolon: s = ( |
            | 
            ((copyRemoveAll
            addSubnode: t)
            addSubnode: e)
            addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         expression = ( |
            | 
            nonLabels asVector at: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         semicolon = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'throwStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         throw = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         tryStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'mySubnodes' From:
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes tryStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: mySubnodes parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         catches.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         finally.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         finallyBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         mySubnodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes tryStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            try: n try.
            tryBlock: n tryBlock.
            catches: n catches.
            finally: n finally.
            finallyBlock: n finallyBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyTry: t TryBlock: tb Catches: catchList = ( |
            | 
            ((copyRemoveAll
              try: t)
              tryBlock: tb)
              catches: catchList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyTry: t TryBlock: tb Catches: catchList Finally: f FinallyBlock: fb = ( |
            | 
            ((((copyRemoveAll
              try: t)
              tryBlock: tb)
              catches: catchList)
              finally: f)
              finallyBlock: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doSubnodes: b = ( |
            | 
            b value: try.
            b value: tryBlock.
            catches do: [|:n| b value: n].
            hasFinally ifTrue: [
              b value: finally.
              b value: finallyBlock.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstSubnode = ( |
            | try).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hasFinally = ( |
            | nil != finally).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lastSubnode = ( |
            | 
            semicolon).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
             n.
            | 
            n: parseKit parseNodes node copyRemoveAll.
            try: n.
            tryBlock: n.
            catches: vector.
            finally: nil.
            finallyBlock: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         subnodes = ( |
             r.
            | 
            r: list copyRemoveAll.
            doSubnodes: [|:s| r addLast: s].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         try.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'tryStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         tryBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         varDclsStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes startsWithModifiers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes varDclsStatement.

CopyDowns:
globals javaParser parseNodes startsWithModifiers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (vector)'
        
         declarators <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes varDclsStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addSubnodesAfterModifiersTo: aList = ( |
            | 
            aList add: resultType.
            aList addAll: declarators.
            aList add: semicolon.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyModifiers: mods Type: resultType Declarators: dcls Semicolon: s = ( |
            | 
            (((copyRemoveAll
            modifiers: mods)
            resultType: resultType)
            declarators: dcls)
            semicolon: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         copySubnodesFrom: n = ( |
            | 
            resend.copySubnodesFrom: n.
            resultType: n resultType copy.
            declarators: n declarators copy.
            semicolon: n semicolon).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeAfterModifiers = ( |
            | 
            resultType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'startsWithModifiers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllSubnodes = ( |
            | 
            resend.removeAllSubnodes.
            resultType: nil.
            declarators: vector.
            semicolon: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> 'parent' -> () From: ( | {
         'Category: outliner support\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsForOutlinerInClassOrInterface: holder = ( |
            | 
            declarators copyMappedBy: [|:d|
              javaParser varDclSlot
                copyClassOrInterface: holder
                    VarDclsStatement: self
                          Declarator: d
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         resultType.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'varDclsStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         semicolon.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: declarations\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         variableDeclarator = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit parseNodes node copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes variableDeclarator.

CopyDowns:
globals parseKit parseNodes node. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         hasInitializer <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes variableDeclarator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addComma: c = ( |
            | addSubnode: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         commaIfPresent: pb IfAbsent: ab = ( |
            | 
            lastSubnode isComma
              ifTrue: [pb value: lastSubnode]
               False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Squares: sqs = ( |
            | 
            (((copyRemoveAll
              addSubnode: n)
              rank: sqs size)
              addAllSubnodes: sqs)
              hasInitializer: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Squares: sqs Equals: equals Initializer: initializer = ( |
             r.
            | 
            r: copyName: n Squares: sqs.
            r hasInitializer: true.
            r addSubnode: equals.
            r addSubnode: initializer.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         equalsAndInitializerIfPresent: pb IfAbsent: ab = ( |
            | 
            hasInitializer
             ifFalse: ab
              True: [ |v|
                v: subnodes asVector.
                pb value: (v at: rank + 1)
                    With: (v at: rank + 2)
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         firstSquareIndex = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | nameNode source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nameNode = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         squares = ( |
            | 
            subnodes asVector copyFrom: firstSquareIndex UpTo: rank + firstSquareIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'variableDeclarator' -> () From: ( | {
         'Comment: number of []\'s\x7fModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         rank <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> () From: ( | {
         'Category: statements\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         whileStatement = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser parseNodes abstractStatement copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> () From: ( |
             {} = 'Comment: The shared, abstract part of parseNode\'s.
A parseNode will typically have construction-specific
data slots, such as thenPart for an if-node.
It also behaves as a collection for its subnodes.
And, it knows the previous and next nodes.\x7fModuleInfo: Creator: globals javaParser parseNodes whileStatement.

CopyDowns:
globals javaParser parseNodes abstractStatement. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser parseNodes whileStatement parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         conditionInParens = ( |
            | 
            subnodeAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyWhile: w ConditionInParens: c Statement: s = ( |
            | 
            ((copyRemoveAll
            addSubnode: w)
            addSubnode: c)
            addSubnode: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'abstractStatement' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statement = ( |
            | lastSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'parseNodes' -> 'whileStatement' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         while = ( |
            | firstSubnode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         javaParseNodes = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaParseNodes.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParseNodes' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'javaNodeSummaries
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addNeededJavaParenthesesOpenProto: op CloseProto: cp = ( |
            | 
            postOrderMapAllSubnodesBy: [|:n. :parent|
              (parent needJavaParenthesesFor: n) ifTrue: [ 
                 [todo unimplementedadd halt and print if debugging].
                 [ ('need parens for: ', parent nodeTypeName, ' containing: ', n nodeTypeName) printLine ].
                javaParser parseNodes parenList copyOpen: op copy InBrackets: self Close: cp copy
              ]
              False: [n].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         allSlots = ( |
            | 
            vector "subnodes").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         categories = ( |
             cache.
             result.
            | 
            result: set copyRemoveAll.
            cache: set copyRemoveAll.

            allSlots do: [ |:s. c |
              c: s category.
              cache if: c IsPresentDo: [] IfAbsentPut: [c] AndDo: [
                | cats |
                cats: s categories.
                cats size > categoryList size  ifTrue: [
                  result add:  cats at: categoryList size.
                ].
              ].
            ].
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         category = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         categoryList = ( |
            | list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         exists = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         isAssignment = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         isCopiedDown = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: emulating slots for outliner\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot'
        
         isFake = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         consumesJavaValue = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaArrayInitializer = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaBlock = ( |
            | isCurlyList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaConstructor = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaIdentifier = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaInitializer = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaKeyword: k = ( |
            | false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaModifier = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaOperator: op = ( |
            | false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaSeparator = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> 'javaTests' -> () From: ( | {
         'ModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isJavaTypeKeyword = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesFor: n = ( |
            | 
            javaPrecedence compare: n javaPrecedence
              IfLess: false
              Equal: [needJavaParenthesesForEqualPrecedenceSubnode: n]
              Greater: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'node' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesesForEqualPrecedenceSubnode: n = ( |
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         couldStartJavaExpression = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         javaPrecedence = 200.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodes' -> 'parenList' -> 'parent' -> () From: ( | {
         'Category: java semantics\x7fModuleInfo: Module: javaParseNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needJavaParenthesisForEqualPrecedenceSubnode: n = ( |
            | 
            false).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'javaNodeSummaries' From: 'applications/klein/javaParser'



 '-- Side effects'

 globals modules javaParseNodes postFileIn
