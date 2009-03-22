# include "theVM.hh"
# include "universe.hh"
# include "stdio.h" //TODO: take this out
# include "verifier.hh"

static Oop oopForTheVM = 0;

void setBootstrapInfo(Oop oop) {
  oopForTheVM = oop;
}

TheVM* theVM() {
  return new TheVM(oopForTheVM);
}

TheVM* setTheVM(int vmOop) {
  oopForTheVM = vmOop;
  return theVM();
}

Universe* TheVM::universe() {
  return new Universe( contentsOfSlotNamed("universe") );
}

bool notyet = true;

void startSmallSelf() {
  printf("Haha! We started smallSelf.\n");
  Verifier v;
  while (notyet) {}
  theVM()->universe()->verify(&v);
  //TODO: get the Self method to run and run it.
}
