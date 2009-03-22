# include "base.hh"
# include <stdio.h>
# include "bytesPartLayout.hh"
# include "byteVectorLayout.hh"

// TODO: These obviously need to be better.
void error         ( const char* msg ) { error_printf_and_flush("error: %s", msg); for(;;) pause(); }
void throwException( const char* msg ) { error(msg); }

void always_assert( bool x, const char* msg ) {
  if (!x)
    error(msg);
}

int roundUpTo( int n, int modulus ) {
  return   (n + modulus - 1)  /  modulus   *   modulus;
}

int lengthOfCString( char* s ) {
  int i = 0;
  for ( char* c = s;  *c != '\0';  ++c)
    ++i;
  return i;
}


bool areCStringsEqual( char* a, char* b ) {
  char* c = a;
  char* d = b;
  while ( *c != '\0' ) {
    if ( *d != *c ) return false;
    ++c;
    ++d;
  }
  return  *d == '\0';
}


bool is_klein_string_equal_to_C_string(int kleinStringSize, Byte* kleinString, char* cString) {
  // There's no '\0' at the end of the kleinString, so be careful.

  for ( int i = 0;  i < kleinStringSize;  ++i) {
    char c = cString[i];
    if ( c == '\0'  ||  c != kleinString[i] ) 
      return false;
  }
  return  cString[kleinStringSize] == '\0';
}


bool isCStringEqualToKleinString( int cStringSize, char* cString, Oop kleinString ) {
  // There's no '\0' at the end of the kleinString, so be careful.
  
  BPRef bpRef = ByteVectorLayout().bytesPartRefOf(kleinString);
  int kleinStringSize = BytesPartLayout().indexableSizeOfBytesPart(bpRef);
  Byte* kleinStringBytes = BytesPartLayout().addressOfFirstByteInBytesPart(bpRef);
  
  if (cStringSize != kleinStringSize) return false;
  
  for (int i = 0; i < cStringSize; i++) {
    if (cString[i] != kleinStringBytes[i]) return false;
  }
  return true;
}

bool is_id_alpha( char c ) {
  return  'a' <= c  &&  c <= 'z'
      ||  'A' <= c  &&  c <= 'Z'
      ||   c == '_';
}


int argCountOfString( char* s, int len )  {
  char c = *s;

  if ( !is_id_alpha(c) )        return 1;

  if ( s[ len - 1 ]  !=  ':' )  return 0; // an optimization

  fint argc = 1;
  for ( const char* ss  =  s + len - 3;  // last is :, next-to-last is alpha
		    ss > s;              // do not need to look at first one
		  --ss )
         if (*ss == ':')  ++argc;

  return argc;
}
