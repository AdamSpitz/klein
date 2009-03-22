# ifndef INTERPRETER_HH
# define INTERPRETER_HH

# include "small_self_types.hh"

class Activation;

class Interpreter {
 public:
  static void        start(oop_t rcvr, oop_t sel);
  static Activation* loop(Activation*);

  static oop_t current_activation();
};


# endif // INTERPRETER_HH