# ifndef KLEIN_BASE_H
# define KLEIN_BASE_H

# define NULL 0

typedef int intNN;
typedef intNN Oop;
typedef intNN* Address;

typedef intNN BPRef;
typedef char Byte;

typedef int MarkValue;
typedef intNN TagValue;

typedef const char* FailureCode;

typedef unsigned char u_char;
typedef int fint;

typedef int LookupType;

typedef Oop ObjVectorOop;
typedef Oop ByteVectorOop;
typedef Oop StringOop;
typedef Oop MapOop;
typedef Oop MethodMapOop;

# define Unused(x) ((x), 0)

# ifdef GENERATE_ASSERTIONS
  # define UsedOnlyInAssert(v)
# else
  inline void UsedOnlyInAssert(void *x) { x, 0; }
# endif

# define SUCCEEDED NULL
# define FAILED "Failed!"


const Address INVALID_ADDRESS   = NULL;
const Oop     INVALID_OOP       = -1;
const int     INVALID_MAP_INDEX = -1;

// AAA TODO: How should we do these?
# define   TRUE_OOP 0
# define  FALSE_OOP 0
# define VECTOR_OOP 0

static const int oopSize = sizeof(Oop);

void error(const char* msg);
void throwException(const char* msg);

void always_assert(bool x, const char* msg);
# define ASSERT(x) always_assert((x), "assertion failure: " #x);

int roundUpTo(int n, int modulus);

int lengthOfCString(char* s);
bool areCStringsEqual( char* a, char* b );
//TODO: eliminate one of these.
bool is_klein_string_equal_to_C_string( int kleinStringSize, Byte* kleinString, char* cString );
bool isCStringEqualToKleinString( int cStringSize, char* cString, Oop kleinString );
int argCountOfString( char* s, int len );

# include "unixDebugServer.hh"

# endif // KLEIN_BASE_H
