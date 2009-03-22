# include "utils.hh"
# include "small_self_types.hh"

// todo cleanup use standard libaries from here 

bool is_id_alpha( char c ) {
  // todo optimize could use a character table lookup -- dmu
  return  'a' <= c  &&  c <= 'z'
      ||  'A' <= c  &&  c <= 'Z'
      ||   c == '_';
}

int length_of_C_string( char* s ) {
  int i = 0;
  for ( char* c = s;  *c != '\0';  ++c)
    ++i;
  return i;
}


int arg_count_of_string( char* s, int len )  {
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