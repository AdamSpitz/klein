 '$Revision: 1.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: FollowSlot'
        
         yodaTests = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaTests.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaTests' -> () From: ( | {
         'ModuleInfo: Module: yodaTests InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'tests' -> () From: ( | {
         'Category: test suites\x7fModuleInfo: Module: yodaTests InitialContents: FollowSlot\x7fVisibility: public'
        
         runYodaSuite = ( |
             errorMessages.
            | 
            [todo cleanup yodaTests errorReporting]. "What the heck is this errorMessages guy doing here?
                                                      I found it in runSelfSuite, but it seems to be useless
                                                      there, too. -- Adam, 5/06"
            errorMessages: list copyRemoveAll.

            scheduler stop.
            startOfTests.

            lookupTest.
            "This one doesn't work yet, but I've been working on it. -- Adam, 5/06"
            [inheritanceTest].

            [todo yodaTests. "Which of these should be included?"
                immediateTest.
                conversionPrologueTest.
                parentInMethodTest.
                diTest.
                unwindProtectTest.
                nlrInliningTest.
                lowLevelTest.
                mirrorTest.
                enumerationTest.
                timeTest.
                stackTest.
                performTest.
                branches run.
                programming run.
                diProgramming run.
                programmingPrims run.
                performConversionTest.
                positionTableTest.
                killTwiceTest.
                sicAllocationTest.
                june2005BugTest.
                [ messageTest        ] twiceWithInlineCacheFlushingBetween.
                [ parserTest         ] twiceWithInlineCacheFlushingBetween.
                _Interpret ifFalse: [ browsingTest. sourceStringTest ].
            ].

            smallIntegerTest.
            arithmeticTest.
            traits smallInt  unitTests run.
            string tokenizingUnitTests run.
            traits indexable unitTests run.

            [todo yodaTests. "Which of these should be included?"
            divAndModTest.
            listTest.
            treeTest.
            vectorTest.
            stringTest.
            textLinesTest.
            sequenceTest.
            orderedSet unitTests run.
            traits universalSetOrDictionary unitTests run.
            messageTest.
            parserTest.
            errorHandlingTest run.
            numberTest.          
            comparator tests doTests.
            sortedSequence tests doTests.
            sharedSetAndDictionaryTests.
            sharedQueueTests.
            orderedDictionaryTests.
            monitorTest.
            mirrorLookupTests.
            browse unitTests run.
            deltablueTest.
            uiTest: errorMessages.
            primitiveMakerTest.
            ].

            endOfTests: errorMessages.
            self).
        } | ) 



 '-- Side effects'

 globals modules yodaTests postFileIn
