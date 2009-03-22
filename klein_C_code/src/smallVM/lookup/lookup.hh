# ifndef KLEIN_LOOKUP_H
# define KLEIN_LOOKUP_H

# include "map.hh"
# include "lookupType.hh"
# include <stdio.h>

class Lookup {
  oop_t selector;
  LookupType lookupType;

  void init(oop_t sel, LookupType lt) { selector = sel; result.rt = Result::foundNone; lookupType = lt; }
  
  void findInObject(oop_t);
  void findInParentsOf(oop_t, MapObj*);
  
 public:  

  class Result {
   public:
    enum result_type { foundNone, foundOne, foundTwo } rt;
    struct SlotReference {
      oop_t holder;
      SlotDesc* sd;
    } refs[2];

    void add_slot(oop_t, SlotDesc*);
    void print() { printf("%s\n", result_type_string()); }
    char* result_type_string() {
      switch(rt) {
       case foundNone: return "foundNone";
       case foundOne:  return "foundOne";
       case foundTwo:  return "foundTwo";
       default: return "???";
      }
    }
  } result;

  static oop_t    findObject( oop_t rcvr,
                              oop_t selector,
                              oop_t* holder_addr = NULL,
                              SlotDesc** sdp = NULL,
                              oop_t* lookup_failed_act_addr = NULL,
                              oop_t sender_act = badOop,
                              bool isUndirectedResend = false,
                              oop_t delegatee = NULL,
                              oop_t holder_of_sender_method = badOop,
                              bool isSelfImplicit = false );

private:

  static Result* findSlotsIn( oop_t rcvr, oop_t selector, LookupType lt );

  static void selectorAndSourceForLookupError (Result::result_type rt, oop_t& sel, char*& source); // UNTESTED
  static oop_t messageTypeForLookupError(LookupType lookupType); // UNTESTED
  static fint argCountForLookupError(oop_t selector, fint perform_arg_count, LookupType lookupType); // UNTESTED
  static oop_t lookupFailed(oop_t rcvr, oop_t sel, oop_t del, oop_t holder_of_sender_method, oop_t sender_act, LookupType lookupType, Result::result_type rt); // UNTESTED
};


# endif // KLEIN_LOOKUP_H
