# ifndef KLEIN_TAG_H
# define KLEIN_TAG_H

# include "base.hh"


class Tag {
 public:
  static const int width = 2;

  static const intNN mask = (1 << width) - 1;

  static const TagValue smi = 0;
  static const TagValue mem = 1;
  static const TagValue flt = 2;
  static const TagValue mark = 3;

  static TagValue tagOfOop( Oop o )   { return o & mask; }
};

# endif // KLEIN_TAG_H
