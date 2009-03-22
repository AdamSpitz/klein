inline void abstract_interpreter_bytecode_info::decode(fint c) {
  code= c;
  op= getOp(code);
  x= getIndex(code);
}


inline abstract_interpreter_interbytecode_state::abstract_interpreter_interbytecode_state() {
  reset_lexical_level();
  reset_index();
  reset_send_modifiers();
  last_literal= NULL;
}
