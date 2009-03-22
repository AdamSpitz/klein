# ifndef TAG_HH
# define TAG_HH

# include "small_self_types.hh"
# include "asserts.hh"

enum tag_t {
    smi_tag = 0,
    mem_tag = 1,
  float_tag = 2,
   mark_tag = 3
};
static const fint tag_mask = 3, tag_shift = 2;

inline oop_t change_tag( oop_t o, tag_t t ) { return (o & ~tag_mask) | t; }

inline tag_t tag(oop_t x)  { return tag_t(x & tag_mask);  }

inline bool is_smi(  oop_t x)  { return tag(x) ==   smi_tag; }
inline bool is_mem(  oop_t x)  { return tag(x) ==   mem_tag; }
inline bool is_float(oop_t x)  { return tag(x) == float_tag; }
inline bool is_mark( oop_t x)  { return tag(x) ==  mark_tag; }


inline oop_t assert_smi  (oop_t x)  { assert(is_smi  (x));  return x; }
inline oop_t assert_mem  (oop_t x)  { assert(is_mem  (x));  return x; }
inline oop_t assert_float(oop_t x)  { assert(is_float(x));  return x; }
inline oop_t assert_mark (oop_t x)  { assert(is_mark (x));  return x; }

inline smi    value_of_smiOop(oop_t x) { return  smi(x) >> tag_shift; }
inline oop_t  smiOop_for_value(smi x)  { oop_t r = oop_t(x) << tag_shift;
                                         assert(value_of_smiOop(r) == x);
                                         return r; }

inline oop_t memOop_for_value(fint x)  { return (oop_t(x) << tag_shift) | mem_tag; }

inline u_smi  value_of_markOop(oop_t x)  { return  smi(x) >> tag_shift; }
inline oop_t  markOop_for_value(u_smi x) { oop_t r = (oop_t(x) << tag_shift) | mark_tag; 
                                           assert(value_of_markOop(r) == x);
                                           return r; }


# endif // TAG_HH
