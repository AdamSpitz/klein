# ifndef KLEIN_BYTECODES_H
# define KLEIN_BYTECODES_H



# define OPWIDTH        4
# define INDEXWIDTH     (8 - OPWIDTH)

# define MAXOP         ((1 << OPWIDTH   ) - 1)
# define MAXINDEX      ((1 << INDEXWIDTH) - 1)

enum ByteCodeKind {
  INDEX_CODE          =  0,  // shift index left, OR in my index field
  LITERAL_CODE        =  1,  // push lits[index]
  SEND_CODE           =  2,  // send w/ receiver on stack,
                             // selector in literals[indx]
  IMPLICIT_SEND_CODE  =  3,  // send w/ implicit receiver,
                             // selector in literals[indx]
  
  NO_OPERAND_CODE     =  4,  // no operand, opcode in index bits, see NoOparandKind
  READ_LOCAL_CODE     =  5,  // read  local; lexical lvl in lex reg, see below
  WRITE_LOCAL_CODE    =  6,  // write local; lexical lvl in lex reg, see below
  LEXICAL_LEVEL_CODE  =  7,  // lexical level of next RW_LOCAL op
  
  BRANCH_CODE         =  8,  // uncond branch via lit
  BRANCH_TRUE_CODE    =  9,  // branch on true via literal
  BRANCH_FALSE_CODE   = 10,  // branch on false via literal
  BRANCH_INDEXED_CODE = 11,  // branch via literal vector, index on stack
  
  DELEGATEE_CODE      = 12,  // delegate next send to lits[index]
  
  ARGUMENT_COUNT_CODE = 13,  // optional bc. for Klein: gives arg. count of next send
  
  // For legacy snapshots, the method uses the old codes (through
  // DELGATEE_CODE) unless it starts with INSTRUCTION_SET_SELECTION_CODE
  // below. -- dmu 10/01
  INSTRUCTION_SET_SELECTION_CODE = ARGUMENT_COUNT_CODE
};

enum NoOperandKind {
  SELF_CODE               = 0,   // push self onto stack
  POP_CODE                = 1,   // pop value from stack
  NONLOCAL_RETURN_CODE    = 2,   // non-local-return from block
  UNDIRECTED_RESEND_CODE  = 3,   // next send is an undirected resend
  END_INIT_CODE           = 4    // marks the end of the initialization section
};

enum InstructionSetKind {
  TWENTIETH_CENTURY_INSTRUCTION_SET = -1, // don't spec = this one
  TWENTIETH_CENTURY_PLUS_ARGUMENT_COUNT_INSTRUCTION_SET = 0,
  KLEIN_INSTRUCTION_SET = 1,
  LAST_INSTRUCTION_SET = KLEIN_INSTRUCTION_SET
};

inline ByteCodeKind getOp(unsigned char c) { return ByteCodeKind(c >> INDEXWIDTH); }
inline fint getIndex(fint c)                    { return c & MAXINDEX; }
inline NoOperandKind getNoOpOp(unsigned char c) {return NoOperandKind(getIndex(c));}

inline bool isSendOp(ByteCodeKind op)   {
  return op == SEND_CODE || op == IMPLICIT_SEND_CODE;
}

inline fint BuildCode(fint op, fint x) {
  assert(x <= MAXINDEX);
  return (op << INDEXWIDTH) | x; 
}


# endif
