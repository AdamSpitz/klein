# include "freeLists.hh"
# include "utils.hh"
# include "objVector.hh"


FreeOops* FreeOopsLists::get_first_entry(ObjVectorObj* freeOopsLists, fint i) {
  assert(smi_tag == 0 && tag_shift == 2); // an untagged address looks like a tagged smi
  return (FreeOops*) freeOopsLists->indexable_at(i);
}


void FreeOopsLists::set_first_entry(ObjVectorObj* freeOopsLists, fint i, FreeOops* f) {
  assert(smi_tag == 0 && tag_shift == 2); // an untagged address looks like a tagged smi
  freeOopsLists->write_indexable_at(i, (oop_t) f);
}


inline fint index_for_freeOopsList_containing_entries_of_size(fint i) {
  return min(i, FreeOopsLists::last_index);
}


void FreeOopsLists::add(ObjVectorObj* freeOopsLists, fint nOops, oop_t* addr) {
  assert(nOops >= FreeOops::oop_size_of_an_entry());
  FreeOops* f = (FreeOops*)addr;
  fint i = index_for_freeOopsList_containing_entries_of_size(nOops);
  f->set_next_freeOops( get_first_entry(freeOopsLists, i) );
  f->set_size(nOops);
  set_first_entry(freeOopsLists, i, f);
}


// todo cleanup: Break up this function? The problem is that all the different sections
//               have to pass more than one thing on to the later sections. -- Adam, 5/06
FreeOops* FreeOopsLists::find_freeOops(ObjVectorObj* freeOopsLists, fint nOops) {
  // find the index of the freeOopsList we need   
  fint i = nOops;
  FreeOops* f;
  while (! (f = get_first_entry(freeOopsLists, i = index_for_freeOopsList_containing_entries_of_size(i)))) {
    if (i == last_index)
      return NULL;
    
    i += (i == nOops
             ? FreeOops::oop_size_of_an_entry()  // don't leave holes too small for a whole freelist entry
             : 1);
  }
  
  smi s = f->size();
  FreeOops* old_f = NULL;
  if (i == last_index) {
    // the last freeOopsList contains all entries of that size or higher,
    // so we need to find an entry of an appropriate size
    while (! FreeOops::is_acceptable_size(nOops, s)) {
      old_f = f;
      f = f->next_freeOops();
      if (!f)
        return NULL;
      s = f->size();
    }
  }
  
  // remove the FreeOops from the list
  if (old_f == NULL)
    set_first_entry(freeOopsLists, i, f->next_freeOops());
  else
    old_f->set_next_freeOops(f->next_freeOops());
  
  // if we used a chunk that was too big, put the extra oops back on the appropriate list
  smi extra_oops = s - nOops;
  if (extra_oops != 0)
    add( freeOopsLists, extra_oops, ((oop_t*) f) + nOops );
  
  return f;
}


void FreeOopsLists::print_freeOops_left(ObjVectorObj* freeOopsLists) {
  for (fint i = FreeOops::oop_size_of_an_entry(); i < last_index; ++i) {
    fint n = 0;
    FreeOops* f = get_first_entry(freeOopsLists, i);
    while (f) {
      ++n;
      f = f->next_freeOops();
    }
    printf("Entries of size %i: %i\n", i, n);
  }

  printf("Sizes of entries of size %i or higher:", last_index);
  FreeOops* f = get_first_entry(freeOopsLists, last_index);
  while (f) {
    printf(" %i,", f->size());
    f = f->next_freeOops();
  }
  printf("\n");
}


// todo adam gc: This code really feels like a hack. It just sorta makes sure that there
//               are a bunch of entries of each size (or higher). What's a better way to
//               decide whether to start a new GC cycle?
const fint DESIRED_ENTRIES_OF_EACH_SIZE = 25; // todo adam gc magic number
bool FreeOopsLists::has_a_fair_amount_of_free_space_left(ObjVectorObj* freeOopsLists) {
  fint entries_needed_of_previous_size = 0;
  for (fint i = FreeOops::oop_size_of_an_entry(); i < last_index; ++i) {
    FreeOops* f = get_first_entry(freeOopsLists, i);
    fint entries_needed_of_this_size = DESIRED_ENTRIES_OF_EACH_SIZE + entries_needed_of_previous_size;
    while (entries_needed_of_this_size && f) {
      --entries_needed_of_this_size;
      f = f->next_freeOops();
    }
    entries_needed_of_previous_size = entries_needed_of_this_size;
  }
  
  FreeOops* f = get_first_entry(freeOopsLists, last_index);
  fint oops_available_in_last_list = 0;
  fint oops_desired = (DESIRED_ENTRIES_OF_EACH_SIZE + entries_needed_of_previous_size) * last_index;
  while (f) {
    oops_available_in_last_list += f->size();
    if (oops_available_in_last_list >= oops_desired)
      return true;
    f = f->next_freeOops();
  }
  return false;
}
