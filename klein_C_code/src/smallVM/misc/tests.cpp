# include "wordLayout.hh"
# include "bytesPartLayout.hh"
# include "markLayout.hh"
# include "byteVectorLayout.hh"
# include "objVectorLayout.hh"
# include "tag.hh"
# include "slotType.hh"
# include "slotTypeFields.incl.hh"
# include "theVM.hh"
# include "universe.hh"
# include "headerFields.hh"
# include "stdio.h"

// A lot of these tests are very brittle and will break if we make any changes to
// the layouts of objects. When that happens, we can either make these tests less
// brittle, or else just throw them away. I mostly just want them for the early
// stages of the project, to give me confidence that this code I'm writing actually
// works. Once we have better end-to-end tests, these tests here will become less
// useful (though maybe still useful enough to be worth debrittlifying them).
// -- Adam, 11/05

void always_assert_equal(int a, int b, char* aMsg, char* bMsg) {
  if ( a != b ) {
    printf("%i != %i\n", a,    b   );
    printf("%s != %s\n", aMsg, bMsg);
    error("assertion failure!");
  }
}

# define ASSERT_EQUAL(a, b) always_assert_equal(a, b, #a, #b)

# define ASSERT_FAIL(x) { try {x  ASSERT(false); } catch (...) {} }

void testTag() {
  ASSERT( Tag::tagOfOop( (Oop)  0 ) == 0 );
  ASSERT( Tag::tagOfOop( (Oop)  1 ) == 1 );
  ASSERT( Tag::tagOfOop( (Oop)  2 ) == 2 );
  ASSERT( Tag::tagOfOop( (Oop)  3 ) == 3 );
  ASSERT( Tag::tagOfOop( (Oop) 16 ) == 0 );
  ASSERT( Tag::tagOfOop( (Oop) 57 ) == 1 );
  ASSERT( Tag::tagOfOop( (Oop) 94 ) == 2 );
  ASSERT( Tag::tagOfOop( (Oop)  7 ) == 3 );
}

void testSlotType() {
  ASSERT( isObjectSlot  ( slotType_slotTypeField_objectSlotValue   << slotType_slotTypeField_shift ) );
  ASSERT( isMapSlot     ( slotType_slotTypeField_mapSlotValue      << slotType_slotTypeField_shift ) );
  ASSERT( isArgumentSlot( slotType_slotTypeField_argumentSlotValue << slotType_slotTypeField_shift ) );

  ASSERT(  isParent(    1 << slotType_isParentField_shift     ) );
  ASSERT( !isParent(    0 << slotType_isParentField_shift     ) );

  ASSERT(  isAssignable(1 << slotType_isAssignableField_shift ) );
  ASSERT( !isAssignable(0 << slotType_isAssignableField_shift ) );

  SlotType assignableObjectSlotType = (1 << slotType_isAssignableField_shift) | (slotType_slotTypeField_objectSlotValue   << slotType_slotTypeField_shift );
  ASSERT( isAssignable(assignableObjectSlotType) );
  ASSERT( isObjectSlot(assignableObjectSlotType) );
}

void testBytesPart() {
  BytesPartLayout bpLayout;
  BPRef bpRef = bpLayout.allocateBytesPartWithIndexableSize(4);
  ASSERT(bpLayout.indexableSizeOfBytesPart(bpRef) == 4);

              bpLayout.forBytesPart_At_Put( bpRef, 0,    'A');
              bpLayout.forBytesPart_At_Put( bpRef, 1,    'd');
              bpLayout.forBytesPart_At_Put( bpRef, 2,    'a');
              bpLayout.forBytesPart_At_Put( bpRef, 3,    'm');

  ASSERT     (bpLayout.forBytesPart_At    ( bpRef, 0) == 'A');
  ASSERT     (bpLayout.forBytesPart_At    ( bpRef, 1) == 'd');
  ASSERT     (bpLayout.forBytesPart_At    ( bpRef, 2) == 'a');
  ASSERT     (bpLayout.forBytesPart_At    ( bpRef, 3) == 'm');

  ASSERT_FAIL(bpLayout.forBytesPart_At    ( bpRef, -1);     );
  ASSERT_FAIL(bpLayout.forBytesPart_At    ( bpRef,  4);     );
}

void testMarkLayout() {
  MarkLayout markLayout;
  ASSERT(markLayout.encode(5) == 23);
  ASSERT(markLayout.decode(99) == 24);
  ASSERT(markLayout.valueOf(79) == 19);
  ASSERT(markLayout.trailingMark() == 3);
  ASSERT(markLayout.hashOfMarkValue(markLayout.set_hashOfMarkValue(17, 56)) == 56);
  ASSERT(markLayout.isMarkValueForByteVector(1));
}

void testMemoryObjectLayout() {
  MemoryObjectLayout moLayout;
  ASSERT(moLayout.memForAddress((Address) 4) == 5);
  ASSERT(moLayout.addressOfMem(9) == (Address) 8);

  Oop o = moLayout.memForAddress(theVM()->universe()->allocateOops(9));

                moLayout.setMarkValueOf(o,   7 );
  ASSERT_EQUAL( moLayout.   markValueOf(o),  7 );
  ASSERT_EQUAL( moLayout.for_At(o, 0)     , 31 );

                moLayout.setOIDOf(o,   13 );
  ASSERT_EQUAL( moLayout.   oidOf(o),  13 );
  ASSERT_EQUAL( moLayout.for_At(o, 1), 52 );
}

void testObjVectorLayout() {
  ObjVectorLayout ovLayout;
  Oop o = ovLayout.memForAddress(theVM()->universe()->allocateOops(15));
  ovLayout.setMarkValueOf( o,   ovLayout.indexableSizeField  ()->markField()->wordMeaningUnencodable()
                              | ovLayout.indexableOriginField()->markField()->wordMeaningUnencodable() );

          ovLayout.set_indexableSizeOf(o,    10);
  ASSERT( ovLayout.    indexableSizeOf(o) == 10);

          ovLayout.set_indexableOriginOf(o,    5);
  ASSERT( ovLayout.    indexableOriginOf(o) == 5);

  ASSERT_EQUAL((int)  ovLayout.for_AddressOfIndexableAt(o, 0),
               (int) (ovLayout.addressOfMem(o) + 5));

                ovLayout.for_IndexableAt_Put( o,  0,   101);
                ovLayout.for_IndexableAt_Put( o,  1,   201);
                ovLayout.for_IndexableAt_Put( o,  2,   301);
                ovLayout.for_IndexableAt_Put( o,  9,  1001);

  ASSERT_EQUAL( ovLayout.for_IndexableAt    ( o,  0),  101);
  ASSERT_EQUAL( ovLayout.for_IndexableAt    ( o,  1),  201);
  ASSERT_EQUAL( ovLayout.for_IndexableAt    ( o,  2),  301);
  ASSERT_EQUAL( ovLayout.for_IndexableAt    ( o,  9), 1001);

  ASSERT_FAIL ( ovLayout.for_IndexableAt    ( o, -1);     );
  ASSERT_FAIL ( ovLayout.for_IndexableAt    ( o, 10);     );
}

void testByteVectorLayout() {
  ByteVectorLayout bvLayout;
  BytesPartLayout bpLayout;
  BPRef bpRef = bpLayout.allocateBytesPartWithIndexableSize(10);
  Oop o = bvLayout.memForAddress(theVM()->universe()->allocateOops(4));

         bvLayout.setMarkValueOf(o,    7);
  ASSERT(bvLayout.   markValueOf(o) == 7);

         bvLayout.set_bytesPartRefOf(o,    bpRef);
  ASSERT(bvLayout.    bytesPartRefOf(o) == bpRef);

                bvLayout.for_IndexableAt_Put( o,  0,  'b' );
                bvLayout.for_IndexableAt_Put( o,  1,  'l' );
                bvLayout.for_IndexableAt_Put( o,  2,  'a' );
                bvLayout.for_IndexableAt_Put( o,  9,  'h' );

  ASSERT_EQUAL( bvLayout.for_IndexableAt    ( o,  0), 'b' );
  ASSERT_EQUAL( bvLayout.for_IndexableAt    ( o,  1), 'l' );
  ASSERT_EQUAL( bvLayout.for_IndexableAt    ( o,  2), 'a' );
  ASSERT_EQUAL( bvLayout.for_IndexableAt    ( o,  9), 'h' );

  ASSERT_FAIL ( bvLayout.for_IndexableAt    ( o, 10);     );
}

int argCountOfCString(char* s) {
  return argCountOfString(s, lengthOfCString(s));
}

void testArgCount() {
  ASSERT_EQUAL( argCountOfCString( "x"          ), 0 );
  ASSERT_EQUAL( argCountOfCString( "x:"         ), 1 );
  ASSERT_EQUAL( argCountOfCString( "blah:"      ), 1 );
  ASSERT_EQUAL( argCountOfCString( "isAardvark" ), 0 );
  ASSERT_EQUAL( argCountOfCString( "to:By:Do:"  ), 3 );
}

void runAllTests() {
  testTag();
  testSlotType();
//  testBytesPart();
//  testMarkLayout();
//  testMemoryObjectLayout();
//  testObjVectorLayout();
//  testByteVectorLayout();
}
