 '$Revision: 30.14 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: stack frames\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         stackFrames = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein stackFrames.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein stackFrames abstract.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein stackFrames abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            klein locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         ppc = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein stackFrames abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein stackFrames ppc.

CopyDowns:
globals klein stackFrames abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () From: ( | {
         'Category: size of pieces\x7fModuleInfo: Module: kleinFrames InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         nonVolLocalRegCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () From: ( | {
         'Category: size of pieces\x7fModuleInfo: Module: kleinFrames InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         nonVolLocalWordCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () From: ( | {
         'Category: size of pieces\x7fModuleInfo: Module: kleinFrames InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         nonVolRegSaveAreaWordCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () From: ( | {
         'Category: size of pieces\x7fModuleInfo: Module: kleinFrames InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         outgoingRcvrAndArgWordCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( |
             {} = 'Comment: from frame_format_ppc.hh

/*

Inside Macintosh: Introduction to PowerPC System Software, 1-44

stack overview:

stack grows down: \\/

                 _________
parameter area            | - caller allocs, and sets
linkage area     _________| - caller allocs and saves its own rtoc here
saved registers           | - callee allocs & saves GPR13 - GPR31 (but C only saves 16-31), FPR14-FPR21
local variables           | - callee allocs and puts locals here
parameter area            | - callee allocs and uses for extra outgoing parms
linkage area     _________| - callee allocs


parameter area: caller allocates this, puts outgoing params in it (after first 8, 
                which go in regs, must fit largest param list called by caller
                
linkage area: caller allocates, caller saves its RTOC here, 
              caller restores its RTOC after every call
              (RTOC value is saved/restored only for cross-toc calls or pointer-based calls)
              callee\'s prologue saves cond reg and link reg here.
              Caller\'s prologue saved pre-caller SP here

linkage area:

incoming sp + 20:            store incoming RTOC (ignored)
incoming sp + 16:            reserved
incoming sp + 12:            reserved
incoming sp +  8:            store incoming link {QUADWORD aligned}
incoming sp +  4:            store incoming CR (if modify cr2 - cr 4) (ignored)
incoming sp:                 caller\'s incoming sp was stored here by caller,
                              QUADWORD aligned, or maybe just doubleword

example: sp -= 192:

incoming sp -  4  sp + 188:  store incoming r31
incoming sp -  8  sp + 184:  store incoming R30
incoming sp - 12  sp + 180:  store incoming R29
incoming sp - 16  sp + 176:  store incoming R28
incoming sp - 20  sp + 172:  store incoming R27
incoming sp - 24  sp + 168:  store incoming R26
incoming sp - 28  sp + 164:  store incoming R25
incoming sp - 32  sp + 160:  store incoming R24
incoming sp - 36  sp + 156:  store incoming R23
incoming sp - 40  sp + 152:  store incoming R22
incoming sp - 44  sp + 148:  store incoming R21
incoming sp - 48  sp + 144:  store incoming R20
incoming sp - 52  sp + 140:  store incoming R19
incoming sp - 56  sp + 136:  store incoming R18
incoming sp - 60  sp + 132:  store incoming R17
incoming sp - 64  sp + 128:  store incoming R16 

since sp -= 192, sp - 64 becomes sp + 128

sp + 116: local 16 (non reg local 0)
....
sp + 108: local N - 1
sp + 104: local 19 (non reg local 3)

sp + 100: last outgoing arg (arg 19)
...
sp + 60: outgoing arg 9
sp + 56: outgoing arg 8 (first nonreg outgoing arg)

sp + 54: space for callee to store of last reg outgoing arg (arg 7, R10)
...
sp + 28: space for callee to store of arg 1, R4
sp + 24: space for callee to store of arg 0, R3

sp + 20: top of linkage area



stack size: 
  6 words in linkage area, 
  max-outgoing-args words for outgoing args
  N locals words
  <alignment to doubleword if any FPRs stored>
  0 - 18 doublewords for saved FPRs (FPR 14 - FRP 31)  
  <alignment so that R31 must end at quadword boundary>
  0 - 56 words for saved GPRs [0 - 19 words] & saved FPRs [0 - 18 doublewords]
  
  so stack size in words =  6 (linkage size) + max-outgoing-args  +  n-nonreg-locals
                         +  fpr-alignment  +  2 * n-saved-fprs
                         +  gpr-alignment  +  n-saved-gprs


R31:  local  0
R30:  local  1
R29:  local  2
R28:  local  3
R27:  local  4
R26:  local  5
R25:  local  6
R24:  local  7
R23:  local  8
R22:  local  9
R21:  local 10
R20:  local 11
R19:  local 12
R18:  local 13
R17:  local 14
R16:  local 15



<incoming sp - 64>

R3:  outgoing arg 0
R4:  outgoing arg 1
R5:  outgoing arg 2
R6:  outgoing arg 3
R7:  outgoing arg 4
R8:  outgoing arg 5
R9:  outgoing arg 6
R10: outgoing arg 7


leaf procs can use red zone, sp-4 through sp-224
  224 is space used by 19 32-bit GPRs + 18 64-bit fprs, rounded up to doubleword boundary


*/

// For these constants, \"top\" means \"farther from the top of stack (TOS)\",
// and \"bottom\" means \"closer to TOS\".  That is, if the stack grows
// downwards in memory (as in Unix), \"top\" pointer > \"bottom\" ptr.


// offsets for saved arg part of stack (parent\'s frame)
inline int32 rcvr_and_argument_offset(fint i /* 0 = rcvr */) {
  return receiver_stack_offset + i;
}

inline int32 stackLocation_offset(fint i, fint max_no_of_outgoing_args_and_rcvr) {
  return receiver_stack_offset + max_no_of_outgoing_args_and_rcvr + i;
}



int32 spOffset(Location reg, nmethod* nm);   // return offset off of sp

// Duplicated in SaveSelfNonVolRegs:
const fint SaveSelfNonVolRegs_frame_size = roundTo(linkage_area_end + 1 
  + NumArgRegisters + NumLocalNonVolRegisters, frame_word_alignment);
\x7fModuleInfo: Creator: globals klein stackFrames ppc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         callersSPOffset = ( |
            | 
            savedSPOffset + totalWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack locations used by runtime, not compiler\x7fComment: // stack offsets from first (lowest) saved non-vol: these are locals that the Self
//  compiler allocates in every one of its frames
// warning current_pc_offset is duplicated in asmDefs_ppc.h for ProfilerTrap

// We cannot put the current_pc word here, as I first thought because:
// when the stack is patched, the original return pc is put in current_pc.
// To find current_pc, we need the nmethod to find how many non-vols are saved in the frame.
// But to find the nmethod we need the original return pc; Catch-22.

// So, keep the current_pc word in the saved_rtoc_field instead.
// After all, that field is only used by the CALLER to do a C-style cross
// RTOC call. (See Inside Macintosh PowerPC System Software.)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         currentPCOffset = ( |
            | savedRTOCOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fComment: align frames to quad word for best load/store multiple speed\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         frameWordAlignment = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: converting offsets to locations\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         ifWordOffset: wordOffset IsLocal: localBlk IsOutgoing: outgoingBlk IsIncoming: incomingBlk = ( |
            | 
            case
              if: (wordOffset >= totalWordCount) Then: [
                [wordOffset >= (totalWordCount + receiverStackOffset)] assert.
                incomingBlk value: wordOffset - totalWordCount - receiverStackOffset
              ]
              If: [wordOffset >= outgoingRcvrAndArgAreaEnd] Then: [
                localBlk value: wordOffset - outgoingRcvrAndArgAreaEnd
              ]
              If: [wordOffset >= receiverStackOffset] Then: [
                outgoingBlk value: wordOffset - receiverStackOffset
              ]
              Else: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack locations used by runtime, not compiler\x7fCategory: stack offsets from first (lowest) saved non-vol\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         lastExtraOffset = ( |
            | 
            nmethodFrameChainOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         lastSavedRegisterOffset = -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         linkageAreaEnd = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         linkageAreaStart = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: offsets for saved arg part of stack (parent\'s frame)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         localSPOffsetAt: i = ( |
            | 
            i >= nonVolLocalWordCount ifTrue: [error: 'what?'].
            i + outgoingRcvrAndArgAreaEnd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: converting offsets to locations\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForStackOffset: byteOffset = ( |
            | 
            ifWordOffset: byteOffset >> 2
                 IsLocal: [|:i| locations nonVolMemoryLocal      copyIndex:        i]
              IsOutgoing: [|:i| locations outgoingMemoryArgument copyRcvrAndArgNo: i]
              IsIncoming: [|:i| locations incomingMemoryArgument copyRcvrAndArgNo: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: offsets for saved arg part of stack (parent\'s frame)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsForNonVolLocals = ( |
            | 
            (vector copySize: nonVolLocalWordCount) mapBy: [|:x. :i|
              locations nonVolMemoryLocal copyIndex: i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: saved non-volatile registers\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsForSavedNonVolRegisters = ( |
             allRegLocs.
            | 
            allRegLocs: theVM myAssemblerSystem allRegisters.
            (vector copySize: nonVolRegSaveAreaWordCount) mapBy: [|:x. :i|
              allRegLocs _At: 31 _IntSub: i.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack locations used by runtime, not compiler\x7fCategory: stack offsets from first (lowest) saved non-vol\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodFrameChainOffset = -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: offsets for saved arg part of stack (parent\'s frame)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         nmethodFrameChainSPOffset = ( |
            | 
            nmethodFrameChainOffset + nonVolRegSaveAreaWordCount negate + totalWordSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack locations used by runtime, not compiler\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodOffset = ( |
            | reserved2Offset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack locations used by runtime, not compiler\x7fCategory: stack offsets from first (lowest) saved non-vol\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         numberExtraLocalsForRuntime = ( |
            | 
            lastExtraOffset negate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: offsets for saved arg part of stack (parent\'s frame)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         outgoingRcvrAndArgAreaEnd = ( |
            | 
            receiverAndArgumentSPOffsetAt: outgoingRcvrAndArgWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: offsets for saved arg part of stack (parent\'s frame)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverAndArgumentSPOffsetAt: i = ( |
            | 
            "0 = rcvr"
            receiverStackOffset + i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverStackOffset = ( |
            | linkageAreaEnd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fCategory: reserving space during compilation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         reserveSpaceForAnotherNonVolLocalReg = ( |
             i.
            | 
            i: nonVolLocalRegCount.
            reserveSpaceForNonVolLocalRegs: i succ.
            i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fCategory: reserving space during compilation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         reserveSpaceForAnotherNonVolMemLocal = ( |
             i.
            | 
            i: nonVolLocalWordCount.
            nonVolLocalWordCount: i succ.
            i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fCategory: reserving space during compilation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         reserveSpaceForNonVolLocalRegs: howMany = ( |
            | 
            reserveSpaceToSaveNonVolRegs: howMany.
            nonVolLocalRegCount: nonVolLocalRegCount max: howMany.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fCategory: reserving space during compilation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         reserveSpaceForOutgoingRcvrAndArgs: howMany = ( |
            | 
            outgoingRcvrAndArgWordCount: outgoingRcvrAndArgWordCount max: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fCategory: reserving space during compilation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         reserveSpaceToSaveNonVolRegs: howMany = ( |
            | 
            nonVolRegSaveAreaWordCount: nonVolRegSaveAreaWordCount max: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         reserved1Offset = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         reserved2Offset = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fComment: store cond reg here before frame creation if you use cr2 - cr4\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         savedCROffset = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fComment: store link reg here before frame creation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         savedPCOffset = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fComment: store RTOC here post frame creation (if trans RTOC)\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         savedRTOCOffset = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stack offsets from stack pointer\x7fCategory: linkage area\x7fComment: store sp here after frame creation\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         savedSPOffset = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: sizing me & my parts\x7fModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         totalWordCount = ( |
             n.
            | 
            n: linkageAreaEnd
             + outgoingRcvrAndArgWordCount
             + nonVolLocalWordCount
             + numberExtraLocalsForRuntime
             + nonVolRegSaveAreaWordCount.

            n roundUpTo:  frameWordAlignment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'stackFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         protoForArchitecture: a = ( |
            | 
            [ppc]. "browsing"
            a sendTo: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         kleinFrames = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinFrames.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinFrames' -> () From: ( | {
         'ModuleInfo: Module: kleinFrames InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinFrames postFileIn
