 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: interface to unixDebugServer\x7fCategory: proxy sockets -- basic marshalling not really KleinAndYoda-specific\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         debuggingProxySocket = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxySocket.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxySocket parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicReadByteIfFail: fb = ( |
            | 
            (socket readOneCharIfFail: [|:e| ^ fb value: e]) asByte).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fComment: reads exactly buf size bytes\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicReadInto: bytes IfFail: fb = ( |
            | 
            socket readInto: bytes Count: bytes size IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicWriteByte: b IfFail: fb = ( |
            | 
            socket write: (byteVector copyAddFirst: b) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicWriteFrom: bytes IfFail: fb = ( |
            | 
            socket write: bytes IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: opening and closing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         close = ( |
            | 
            closeIfFail: []).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: opening and closing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         closeIfFail: fb = ( |
            | 
            socket ifNil: [^ self].
            socket closeIfFail: [|:e| ^ fb value: e].
            socket: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy sema: sema copyBinary).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: flushing (placeholders)\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         discardBufferContents = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: flushing (placeholders)\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         flushOutputIfFail: fb = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: opening and closing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         open: hostName Port: port = ( |
            | 
            open: hostName Port: port IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: opening and closing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         open: hostName Port: port IfFail: fb = ( |
            | 
            (copy 
             socket:
              os_file openTCPHost: hostName
                             Port: port
                           IfFail: fb)
             discardBufferContents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         readByteIfFail: fb = ( |
            | 
            safelyDo: [
              basicReadByteIfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         readBytesIfFail: fb = ( |
            | 
            safelyDo: [
              readBytesOrStringLike: byteVector IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         readBytesOrStringLike: proto IfFail: fb = ( |
             len.
             s.
            | 
            len: readLengthIfFail: [|:e| ^ fb value: e].
            len > 100000000  ifTrue: [
               ^ fb value: 'protocol error: ridiculously large length: 16r', 
                           len shortIfPossibleHexPrintString
            ].
            s: proto copySize: len.
            basicReadInto: s 
                    IfFail: [|:e| ^ fb value: 'could not read contents: ', e].
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         readIntArrayIfFail: fb = ( |
            | 
            safelyDo: [
              | len. res |
              len: readIntIfFail: [|:e| ^ fb value: e].
              res: vector copySize: len.
              len do: [|:i|
                res at: i Put: readIntIfFail: [|:e| ^ fb value: e]
              ].
              res
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         readIntIfFail: fb = ( |
            | 
            safelyDo: [
              | a. b. c. d |
              a: int32 shl: (basicReadByteIfFail: [|:e| ^ fb value: e]) With: 24.
              b: int32 shl: (basicReadByteIfFail: [|:e| ^ fb value: e]) With: 16.
              c: int32 shl: (basicReadByteIfFail: [|:e| ^ fb value: e]) With:  8.
              d:             basicReadByteIfFail: [|:e| ^ fb value: e].
              int32 or: a With: int32 or: b With: int32 or: c With: d
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         readLengthIfFail: fb = ( |
            | 
            safelyDo: [
              readIntIfFail: [|:e| ^ fb value: 'could not read length: ', e]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: reading\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         readStringIfFail: fb = ( |
            | 
            safelyDo: [
              readBytesOrStringLike: mutableString IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: serializing\x7fComment: protocol cannot handle interleaving so
enforce serialization\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         safelyDo: blk = ( |
            | 
            sema protect: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeByte: b IfFail: fb = ( |
            | 
            safelyDo: [
              basicWriteByte: b asByte IfFail: [|:e| ^ fb value: e].
              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeBytes: bytes IfFail: fb = ( |
            | 
            safelyDo: [
              writeLength: bytes size IfFail: [|:e| ^ fb value: e].
              basicWriteFrom: bytes IfFail: [|:e| ^ fb value: e].
              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fComment: Assumes a byte array that represent int32 data\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeInt32ByteArray: a IfFail: fb = ( |
            | 
            (a size % os bytesPerInt32) != 0 ifTrue: [ ^ fb: 'byte array does not seem to represent int32']. 
            safelyDo: [
              writeInt: (a size / os bytesPerInt32) IfFail: [|:e| ^ fb value: e].
              a do: [|:i| writeByte: i IfFail: [|:e| ^ fb value: e]].
              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeInt: n IfFail: fb = ( |
            | 
            safelyDo: [
              basicWriteFrom:  (int32 asBigEndianByteVectorFrom: n)
                      IfFail:  [|:e| ^ fb value: e].
              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeIntArray: a IfFail: fb = ( |
            | 
            safelyDo: [
              writeInt: a size IfFail: [|:e| ^ fb value: e].
              a do: [|:i| writeInt: i IfFail: [|:e| ^ fb value: e]].
              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeLength: len IfFail: fb = ( |
            | 
            safelyDo: [
              writeInt: len IfFail: [|:e| ^ fb value: 'could not write length: ', e]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         writeString: s IfFail: fb = ( |
            | 
            safelyDo: [
              writeBytes: s IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (recursiveSemaphore copyBinary)'
        
         sema <- recursiveSemaphore copyBinary.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (nil)'
        
         socket.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: interface to unixDebugServer\x7fCategory: proxy sockets -- basic marshalling not really KleinAndYoda-specific\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         bufferedDebuggingProxySocket = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda debuggingProxySocket copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda bufferedDebuggingProxySocket.

CopyDowns:
globals kleinAndYoda debuggingProxySocket. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (byteVector copySize: 10000)\x7fVisibility: private'
        
         inputBuffer <- byteVector copySize: 10000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         inputLength <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nextInput <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nextOutput <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'Category: buffering\x7fModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (byteVector copySize: 10000)\x7fVisibility: private'
        
         outputBuffer <- byteVector copySize: 10000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda bufferedDebuggingProxySocket parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicReadByteIfFail: fb = ( |
             r.
            | 
            fillInputIfFail: [|:e| ^ fb value: e].
            r: inputBuffer at: nextInput.
            nextInput: nextInput succ.
            inputLength: inputLength pred.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicReadInto: bytes IfFail: fb = ( |
             dstPos <- 0.
             total <- 0.
            | 
            [total < bytes size] whileTrue: [|len|
              fillInputIfFail: [|:e| ^ fb value: e].
              len: bytes size - dstPos  min:  inputLength.
              bytes copyRangeDstPos: dstPos
                           SrcArray: inputBuffer
                             SrcPos: nextInput
                                Len: len.
                   dstPos: dstPos      + len.
                    total: total       + len.
                nextInput: nextInput   + len.
              inputLength: inputLength - len.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicWriteByte: b IfFail: fb = ( |
            | 
            == prototype ifTrue: [error: 'proxy corruption'].
            nextOutput  =  outputBuffer size  ifTrue: [flushOutput].
            outputBuffer at: nextOutput Put: b asByte.
            nextOutput: nextOutput succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: basic reading & writing\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         basicWriteFrom: bytes IfFail: fb = ( |
             srcPos <- 0.
             total <- 0.
            | 
            == prototype ifTrue: [error: 'proxy corruption'].
            [|len|
              len: bytes size - srcPos  min:  outputBuffer size - nextOutput.
              outputBuffer copyRangeDstPos: nextOutput
                                  SrcArray: bytes
                                    SrcPos: srcPos
                                       Len: len.
              total: total + len.
              srcPos: srcPos + len.
              nextOutput: nextOutput + len.
              total = bytes size  ifTrue: [^ self].
              flushOutputIfFail: [|:e| ^ fb value: e].
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (resend.copy 
            inputBuffer: inputBuffer copy)
            outputBuffer: outputBuffer copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: filling & flushing buffers\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         discardBufferContents = ( |
            | 
            nextOutput: 0.
            nextInput: 0.
            resend.discardBufferContents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: filling & flushing buffers\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot'
        
         ensureOutputBufferIsEmpty = ( |
            | 
            ensureOutputBufferIsEmpty: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: filling & flushing buffers\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot'
        
         ensureOutputBufferIsEmpty: fb = ( |
            | 
            isOutputBufferEmpty
             ifFalse: [fb value: 'output buffer should be empty but is not']
                True: [self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: filling & flushing buffers\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInputIfFail: fb = ( |
            | 
            inputLength = 0  ifFalse: [^ self].
            inputLength:
              socket readInto: inputBuffer Min: 1 Max: inputBuffer size
                       IfFail: [|:e| ^ fb value: 'could not fill buffer: ', e].
            nextInput: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: filling & flushing buffers\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         flushOutputIfFail: fb = ( |
            | 
            safelyDo: [
              socket ifNil: [ ^ fb value: 'no socket' ].
              socket writeFrom: outputBuffer Count: nextOutput IfFail: [|:e| ^ fb value: e].
              nextOutput: 0
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'Category: filling & flushing buffers\x7fModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot'
        
         isOutputBufferEmpty = ( |
            | nextOutput = 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxySocket' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'bufferedDebuggingProxySocket' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            kleinAndYoda bufferedDebuggingProxySocket).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot'
        
         vmKitProxySockets = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitProxySockets.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxySockets' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxySockets InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitProxySockets postFileIn
