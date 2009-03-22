# include "errorCodes.hh"
# include "stringObj.hh"




ErrorCodeEntry ErrorCodes::errorTable[] = {
  { PRIMITIVENOTDEFINEDERROR, "primitiveNotDefinedError" },
  { PRIMITIVEFAILEDERROR,         "primitiveFailedError" },
  
  { BADTYPEERROR,               "badTypeError" },
  { DIVISIONBYZEROERROR, "divisionByZeroError" },
  { OVERFLOWERROR,             "overflowError" },
  { BADSIGNERROR,               "badSignError" },
  { ALIGNMENTERROR,           "alignmentError" },
  { BADINDEXERROR,             "badIndexError" },
  { BADSIZEERROR,               "badSizeError" },
  { REFLECTTYPEERROR,       "reflectTypeError" },
  
  { OUTOFMEMORYERROR,     "outOfMemoryError" },
  { STACKOVERFLOWERROR, "stackOverflowError" },
  
  { SLOTNAMEERROR,                         "slotNameError" },
  { BADSLOTNAMEERROR,                   "badSlotNameError" },
  { ARGUMENTCOUNTERROR,               "argumentCountError" },
  { PARENTERROR,                             "parentError" }, //illegal parent priority
  { UNASSIGNABLESLOTERROR,         "unassignableSlotError" },
  { LONELYASSIGNMENTSLOTERROR, "lonelyAssignmentSlotError" },
  
  
  //activation errors
  { NOACTIVATIONERROR,  "noActivationError" }, //dead activation
  { NORECEIVERERROR,      "noReceiverError" }, //activation has no receiver
  { NOPARENTERROR,          "noParentError" }, //  ''              parent
  { NOSENDERERROR,          "noSenderError" }, //  ''              sender
  
  //invalid branch bytecode
  { BADBRANCHERROR, "badBranchError" },
  
  //marks end of errorTable
  { ENDMARKER , "" }
};

oop_t getSelfString(ErrorCodeEntry* e) {
   return e->selfString ? e->selfString : StringObj::intern(e->cString);
}

void ErrorCodes::initialize() {
  for (ErrorCodeEntry *e = errorTable;  !e->end_marker();  ++e) {
    e->selfString = StringObj::intern(e->cString);
  }
}

// todo optimize time 
oop_t ErrorCodes::stringForError (error_t errorID) {
  for (ErrorCodeEntry *e = errorTable; !e->end_marker(); ++e)
    if (e->ID == errorID) return getSelfString(e);
  unimplemented("missing error string error");
  return 0;
}