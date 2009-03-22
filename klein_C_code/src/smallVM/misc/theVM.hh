# ifndef KLEIN_THE_VM_H
# define KLEIN_THE_VM_H

# include "kleinObject.hh"

class Universe;

class TheVM : public KleinObject {
 public:
  TheVM(Oop oop) : KleinObject(oop) {}
  Universe* universe();
};

TheVM* theVM();


# endif // KLEIN_THE_VM_H
