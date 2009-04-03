 '$Revision: 30.28 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: lookup info\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         sendDescs = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein sendDescs.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein sendDescs abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         ppc = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein sendDescs ppc.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         delegateeIndex = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         gcMaskIndex = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: gc mask\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         gcMaskLayout = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein sendDescs ppc gcMaskLayout.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         ifLocation: loc ShouldBeRepresentedInGCMask: maskBlk Else: elseBlk = ( |
            | 
            [todo optimization stackRoots]. "Someday it might be worthwhile to figure out
                                             which locations would be most useful to represent
                                             in the gcMask. But for now, let's just only
                                             represent the registers, and not bother with any
                                             stack locations. -- Adam, 9/05"
            case
              if: [loc isRegister] Then: [maskBlk value: loc register number]
                                   Else: elseBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            myAssemblerSystem intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         locationRepresentedByBit: i = ( |
            | 
            locations register copyForRegister: myAssemblerSystem operands gprFor: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            klein locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         maskForLocations: locs = ( |
             m <- 0.
            | 
            locs do: [|:loc|
              ifLocation: loc ShouldBeRepresentedInGCMask: [|:bitNumber|
                m:  intNN or:  m  With:  intNN maskForBitAt: bitNumber.
              ] Else: [].
            ].
            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | 
            assemblerSystems ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> 'gcMaskLayout' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldLocationBeRepresentedInGCMask: loc = ( |
            | 
            ifLocation: loc ShouldBeRepresentedInGCMask: true Else: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: compilation\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         generateBackpatchAndFlushMachineCaches: sendDescReg TargetTo: targReg Map: mapReg Temp1: t1 Temp2: t2 With: cg = ( |
            | 
            cg a  stwFrom: mapReg Disp: previousMapIndex * cg oopSize Base: sendDescReg.

            cg generateBackpatch:                           sendDescReg      Offset: retryIndex NewAddress: targReg        Temp1: t1 Temp2: t2.
            cg generateFlushMachineCachesAfterBackpatching: sendDescReg  FromOffset: retryIndex ToOffset: previousMapIndex Temp1: t1 Temp2: t2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: compilation\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCallStubName: stubName LookupKey: key LiveOopTracker: aLiveOopTracker With: cg = ( |
             a.
             branchStart.
             indexPastMe.
             isMessageSend.
             labelPastMe.
             nlr.
             start.
            | 
            "see CodeGen::SendDesc"
            a: cg a.
            isMessageSend: key isNotNil.

            cg shouldZapDeadLocations ifTrue: [aLiveOopTracker zapDeadLocations].

            branchStart: a locationCounter.
            cg branchToStubName: stubName UsingCTRAnd: a r0 SetLink: true.
            start: a locationCounter.
            (branchStart - start) = (cg oopSize * retryIndex) ifFalse: [error: 'should agree'].

            indexPastMe:  isMessageSend ifTrue: [size] False: [gcMaskIndex succ].

            "Small optimization: define the pastme label in the correct position right now,
             because maintaining the set of unresolvedUses and then resolving it is
             slightly expensive. -- Adam, 11/04"
            labelPastMe: a defineLabelAt: start + (cg oopSize * indexPastMe).

            a locationCounter:   start + (cg oopSize * normalReturnIndex).  a bDisp: labelPastMe.
            a locationCounter:   start + (cg oopSize *       gcMaskIndex).  a data32: aLiveOopTracker gcMask.

            isMessageSend ifTrue: [
              a locationCounter: start + (cg oopSize *  previousMapIndex).  a data32: 0.
              a locationCounter: start + (cg oopSize *         nmlnIndex).  a data32: 0.
                                                                            a data32: 0. [todo nmln].
              a locationCounter: start + (cg oopSize *     selectorIndex).  cg assembleObject: key selector.
              a locationCounter: start + (cg oopSize *   lookupTypeIndex).  cg assembleObject: key lookupType.
              a locationCounter: start + (cg oopSize *    nlrReturnIndex).  nlr: a newLabel. a bDisp: nlr.
              a locationCounter: start + (cg oopSize *    delegateeIndex).  cg assembleObject: key delegatee.
            ].

            a locationCounter:   start + (cg oopSize *       indexPastMe).
            nlr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         lookupTypeIndex = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         nlrReturnIndex = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         nmlnIndex = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         nmlnPart2Index = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         normalReturnIndex = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         previousMapIndex = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         retryIndex = -4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         savedPCIndex = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         selectorIndex = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'sendDescs' -> 'ppc' -> () From: ( | {
         'Category: layout\x7fModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         size = ( |
            | 
            delegateeIndex succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         kleinSendDesc = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinSendDesc.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.28 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinSendDesc InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinSendDesc postFileIn
