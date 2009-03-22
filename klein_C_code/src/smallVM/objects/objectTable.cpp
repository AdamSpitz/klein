# include "objectTable.hh"
# include "objVector.hh"

ObjVectorObj*          Object_Table::ot_addr;
MemObj**               Object_Table::base;
fint                   Object_Table::size;
int32                  Object_Table::timestamp = 0;

void Object_Table::initialize( ObjVectorObj* a ) {
  ot_addr = a;
  base    = (MemObj**) a->indexable_addr(0);
  size    =            a->indexableSize();
}

// todo optimize time: Could just keep lastInvalidEntry as a C++ variable, but then we'd have to make sure to update the slot
//                     in the objectLocator object whenever necessary. -- Adam, 5/06
// optimization: bypass the write barrier
oop_t Object_Table::       lastInvalidEntry(       ) { return          ot_addr->read_oop(lastInvalidEntry_offset);      }
void  Object_Table::record_lastInvalidEntry(oop_t o) { ++timestamp;  *(ot_addr->oop_addr(lastInvalidEntry_offset)) = o; }
