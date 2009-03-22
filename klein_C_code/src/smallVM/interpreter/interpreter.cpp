# include "interpreter.hh"
# include "lookup.hh"
# include "prims.hh"
# include "activation.hh"
# include "errorCodes.hh"
# include <stdlib.h>


void runAllTests() {
}

void startSmallSelf() {
  Interpreter::start( The::oop_of(The::vm), The::oop_of(The::start_selector) );
  printf_and_flush("Interpreter exited normally\n");
  exit(0);
}

oop_t Interpreter::current_activation() { return The::oop_of(The::active_context); }

void Interpreter::start(oop_t rcvr, oop_t sel) {
    printf_and_flush("starting interpreter\n");

    Primitives::initialize();
    ErrorCodes::initialize();

    oop_t holder;
    oop_t contents = Lookup::findObject( rcvr, sel, &holder );

    if (!is_method(contents))
      fatal("start method?");
      
    oop_t new_activation, a;
    for ( a = ActivationObj::clone_for(contents, holder, rcvr, NULL, 0, NULL);
          a;
          a = new_activation )
      new_activation = ActivationObj::from(a)->loop(a);
}
