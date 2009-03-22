# include <stdlib.h>
# include "asserts.hh"

void fatal_handler( char* file, int line, char* msg) {
  error_printf_and_flush("fatal %s:%d %s", file, line, msg);
  abort();
}  


void untested_handler( char* file, int line, char* msg) {
  error_printf_and_flush("untested %s:%d %s", file, line, msg);
}  


void unimplemented_handler( char* file, int line, char* msg) {
  error_printf_and_flush("unimplemented %s:%d %s", file, line, msg);
  abort();
}  