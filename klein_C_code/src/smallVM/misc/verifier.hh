# ifndef KLEIN_VERIFIER_H
# define KLEIN_VERIFIER_H

# include "base.hh"

class Verifier {
 public:
  void always_assert( bool b, char* msg );
};

# endif // KLEIN_VERIFIER_H
