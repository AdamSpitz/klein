# include "verifier.hh"

void Verifier::always_assert(bool b, char* msg) {
  if (!b)  throwException(msg);
}
