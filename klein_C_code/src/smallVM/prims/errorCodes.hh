# ifndef ERROR_CODES_HH
# define ERROR_CODES_HH

# include "small_self_types.hh"

enum error_t {
PRIMITIVENOTDEFINEDERROR,
PRIMITIVEFAILEDERROR,
BADTYPEERROR,
DIVISIONBYZEROERROR,
OVERFLOWERROR,
BADSIGNERROR,
ALIGNMENTERROR,
BADINDEXERROR,
BADSIZEERROR,
REFLECTTYPEERROR,
OUTOFMEMORYERROR,
STACKOVERFLOWERROR,
SLOTNAMEERROR,
BADSLOTNAMEERROR,
ARGUMENTCOUNTERROR,
PARENTERROR,
UNASSIGNABLESLOTERROR,
LONELYASSIGNMENTSLOTERROR,
NOACTIVATIONERROR,
NORECEIVERERROR,
NOPARENTERROR,
NOSENDERERROR,
BADBRANCHERROR,
ENDMARKER
};

class ErrorCodeEntry {
  public:
    error_t ID;
    char* cString;
    oop_t selfString; //init to -1, and cache?
  public:
    static oop_t getSelfString();
    bool end_marker() { return (ID == ENDMARKER) ? true : false; }
};


class ErrorCodes {
  private:
    static ErrorCodeEntry errorTable[];
  
  public:
    static void initialize();
    static oop_t stringForError(error_t errorID);
};


# endif // ERROR_CODES_HH