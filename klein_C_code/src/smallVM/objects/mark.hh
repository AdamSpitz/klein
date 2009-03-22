# ifndef MARK_HH
# define MARK_HH

# include "tag.hh"


static const fint mark_byteVector_shift              = 0;
// todo adam gc: Do we really need a bit to identify activationMaps, now that we've got one to identify activations?
static const fint mark_activationMap_shift           = 1;
static const fint mark_activation_shift              = 2;
static const fint mark_hasBeenVisitedForLookup_shift = 3;
static const fint mark_hasBeenVisitedForGC_shift     = 4;
static const fint mark_isOnMarkStackForGC_shift      = 5;
static const fint mark_isInRememberedSetForGC_shift  = 6;
static const fint mark_oid_shift                     = 7;
static const smi  mark_oid_width                     = sizeof(oop_t) * 8 - mark_oid_shift;
static const smi  mark_byteVector_bit              = 1 << mark_byteVector_shift;
static const smi  mark_activationMap_bit           = 1 << mark_activationMap_shift; // todo use an enum, so that we don't take up so many bits
static const smi  mark_activation_bit              = 1 << mark_activation_shift;
static const smi  mark_hasBeenVisitedForLookup_bit = 1 << mark_hasBeenVisitedForLookup_shift;
static const smi  mark_hasBeenVisitedForGC_bit     = 1 << mark_hasBeenVisitedForGC_shift;
static const smi  mark_isOnMarkStackForGC_bit      = 1 << mark_isOnMarkStackForGC_shift;
static const smi  mark_isInRememberedSetForGC_bit  = 1 << mark_isInRememberedSetForGC_shift;
static const smi  mark_oid_mask                    = ((1 << mark_oid_width) - 1) << mark_oid_shift;

static const oop_t  markOop_activation_bit = mark_activation_bit << tag_shift;
static const oop_t  markOop_hasBeenVisitedForLookup_bit = mark_hasBeenVisitedForLookup_bit << tag_shift;
static const oop_t  markOop_hasBeenVisitedForGC_bit = mark_hasBeenVisitedForGC_bit << tag_shift;
static const oop_t  markOop_isOnMarkStackForGC_bit = mark_isOnMarkStackForGC_bit << tag_shift;
static const oop_t  markOop_isInRememberedSetForGC_bit = mark_isInRememberedSetForGC_bit << tag_shift;
static const oop_t  markOop_oid_mask = mark_oid_mask << tag_shift;

static const fint markOop_oid_shift = mark_oid_shift + tag_shift;

inline oop_t adjust_mark_oop_to_encode_oid( oop_t mark_oop, smi oid ) { return (oid << markOop_oid_shift) | (mark_oop & ~markOop_oid_mask); }

inline oop_t gc_make_markOop_black(oop_t mark_oop) { return (mark_oop |  markOop_hasBeenVisitedForGC_bit) & ~markOop_isOnMarkStackForGC_bit; }
inline oop_t gc_make_markOop_grey( oop_t mark_oop) { return  mark_oop |  markOop_hasBeenVisitedForGC_bit  |  markOop_isOnMarkStackForGC_bit; }
inline oop_t gc_make_markOop_white(oop_t mark_oop) { return  mark_oop & ~markOop_hasBeenVisitedForGC_bit  & ~markOop_isOnMarkStackForGC_bit; }

inline  bool gc_is_markOop_black(  oop_t mark_oop) { return   (mark_oop & markOop_hasBeenVisitedForGC_bit) && ! (mark_oop & markOop_isOnMarkStackForGC_bit); }
inline  bool gc_is_markOop_white(  oop_t mark_oop) { return ! (mark_oop & markOop_hasBeenVisitedForGC_bit);                                                  }


static const oop_t badOop          = markOop_for_value(~0);
static const oop_t trailingMarkOop = markOop_for_value(0);

                                         
# endif // MARK_HH
