# include "objectLayout.hh"
# include "tag.hh"
# include "memoryObjectLayout.hh"


Oop ObjectLayout::mapOf( Oop o ) {
  TagValue t = Tag::tagOfOop(o);
  switch (t) {
    case Tag::mem  : { return MemoryObjectLayout().mapOf(o); }
    case Tag::smi  : error("smi"              ); break;
    case Tag::flt  : error("float"            ); break;
    case Tag::mark : error("smi"              ); break;
    default:         error("unknown tag value");
  }
  return 0;
}
