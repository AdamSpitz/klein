 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot'
        
         parseKitInput = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitInput.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitInput' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fCategory: position tracking\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         inputExtent = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit inputExtent.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fCategory: position tracking\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         inputPosition = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> () From: ( |
             {} = 'Comment: A position in an input stream:
line, column, and byte-offset.\x7fModuleInfo: Creator: globals parseKit inputPosition.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (parseKit inputPosition)\x7fVisibility: private'
        
         myEnd <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (parseKit inputPosition)\x7fVisibility: private'
        
         myStart <- bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( |
             {} = 'Comment: I am a functional object
tracking a start and end.\x7fModuleInfo: Creator: globals parseKit inputExtent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         < ie = ( |
            | start < ie start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         = ie = ( |
            | 
            (start = ie start)
            && [ end = ie end]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         end = ( |
            | myEnd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         end: inputPos = ( |
            | 
            copy myEnd: inputPos).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            start hash ^^ end hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | myStart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         start: inputPos = ( |
            | copy myStart: inputPos).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         startAndEnd: inputPos = ( |
            | 
            (copy start: inputPos) end: inputPos).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         statePrintString = ( |
            | 
            '[', start statePrintString, ', ', end statePrintString, ']').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputExtent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         testResultString = ( |
            | '[', start asPoint printString, ', ', end asPoint printString, ']').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (1)'
        
         column <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (1)'
        
         line <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (0)'
        
         offset <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit inputPosition parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | offset < x offset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | offset = x offset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         asPoint = ( |
            | column @ line).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         copyColumn: c Line: l Offset: o = ( |
            | 
            ((clone
            column: c)
            line: l)
            offset: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | offset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         statePrintString = ( |
            | 
            offset printString, '(', asPoint printString, ')').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         succ = ( |
            | 
            "only works if I am not pointing to a newline"
            copyColumn: column succ Line: line Offset: offset succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputPosition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         to: anInputPosition = ( |
            | 
            (parseKit inputExtent copy myStart: self) myEnd: anInputPosition).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fCategory: position tracking\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         inputStream = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit inputStream.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (1)\x7fVisibility: private'
        
         column <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         input <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isForFile <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (1)\x7fVisibility: private'
        
         line <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nextOffset <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( |
             {} = 'Comment: This object offers 2 character lookahead,
peek, and peekSecond.
next does a peek and advance,
and the position of the peek char and the previous
character are both available.\x7fModuleInfo: Creator: globals parseKit inputStream parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         advance = ( |
             c.
            | 
            previousColumn: column.
            previousOffset: nextOffset.

            isAtCRLF ifTrue: [ nextOffset: nextOffset + 2. ^ advanceToNewLine].

            c: input at: nextOffset IfAbsent: ' '.
            nextOffset: nextOffset succ.
            '\n' = c ifTrue:  [ ^ advanceToNewLine ].
            '\r' = c ifTrue:  [ ^ advanceToNewLine ].
            column: column succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         advance: nChars = ( |
            | 
            nChars do: [advance]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         advanceToNewLine = ( |
            | 
            line: line succ.
            column: 1.
            shouldPrintDots ifTrue: ['.' print].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         close = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForFileNamed: fn = ( |
            | 
            copyForFileNamed: fn IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForFileNamed: fn IfFail: fb = ( |
             f.
             r.
            | 
            f: os_file openForReading: fn IfFail: [|:e| ^ fb value: e].
            r: f contents.
            f close.
            (copyForString: r) isForFile: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForString: s = ( |
            | 
            clone init input: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         eofChar = '\x00'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         goBack = ( |
            | 
            nextOffset: nextOffset pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         init = ( |
            | 
            column: 1.
            input: ''.
            line: 1.
            nextOffset: 0.
            previousColumn: 0.
            previousOffset: -1.
            shouldPrintDots: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: private'
        
         isAtCRLF = ( |
            | 
            ((input at: nextOffset IfAbsent: [^ false]) = '\r')
            &&
            [(input at: nextOffset succ  IfAbsent: [^  false]) = '\n']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         next = ( |
            | 
            nextIfEOF: eofChar).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         nextIfEOF: blk = ( |
             r.
            | 
            r: input at: nextOffset IfAbsent: [^ blk value].
            advance.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         peek = ( |
            | 
            peekIfEOF: eofChar).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         peekAtMost: s = ( |
             len.
            | 
            "return the next s characters, or fewer if hit EOF"
            len: 0 max:  s min: input size - nextOffset.
            input copyFrom: nextOffset UpTo: nextOffset + len).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         peekIfEOF: blk = ( |
            | 
            input at: nextOffset  IfAbsent: [^ blk value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         peekSecond = ( |
            | 
            peekSecondIfEOF: eofChar).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         peekSecondIfEOF: blk = ( |
            | 
            input   at: nextOffset + (isAtCRLF ifTrue: 2 False: 1)
              IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         position = ( |
            | 
            parseKit inputPosition
              copyColumn: column
                    Line: line
                  Offset: nextOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         previous = ( |
            | previousIfFirst: eofChar).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         previousIfFirst: fb = ( |
            | 
            input at: nextOffset pred IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: positioning\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         previousPosition = ( |
             pl.
            | 
            pl: previousColumn succ = column  
                  ifTrue: line  False: [line pred].
            parseKit inputPosition
              copyColumn: previousColumn
                    Line: pl
                  Offset: previousOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: source accessing\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceAt: anInputExtent = ( |
             e.
             s.
            | 
            s: anInputExtent start offset.
            e: anInputExtent end   offset.
            s: s max: 0.
            e: e min: input size pred.
            s <= e  ifFalse: [^ '???'].
            input copyFrom: s UpTo: e succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> 'parent' -> () From: ( | {
         'Category: scaffolding\x7fModuleInfo: Module: parseKitInput InitialContents: FollowSlot'
        
         test = ( |
             s.
            | 
            s: copyForFileNamed: 'applications/javaParser/test.java'.
            60 do: [| n. p. pos |
              pos: s nextCharPosition.
              p: s peek.
              n: s next.
              ('next: ', n storeString, '  peek: ', p storeString, '  pos: ', pos printString)
                printLine.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         previousColumn <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         previousOffset <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'inputStream' -> () From: ( | {
         'ModuleInfo: Module: parseKitInput InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         shouldPrintDots <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 



 '-- Side effects'

 globals modules parseKitInput postFileIn
