# ifndef KLEIN_SMALL_VM_DECLARATIONS_OR_STUBS_H
# define KLEIN_SMALL_VM_DECLARATIONS_OR_STUBS_H

// $Revision: 1.10 $

// Contains stubs or declarations for the smallVM -- David Ungar 12/05

# ifdef SMALL_VM

  static const  int fixed_address_buffer_length = 10 * 1024 * 1024;

  static const int default_port = 9091;
  static const char initiation_request[]  =  "request_small_self_1.5";
  static const char initiation_response[] =  "respond_small_self_1.5";
  
  void runAllTests();

  class TheVM;
  void   setBootstrapInfo(int vmOop, int objectTableAddr);
  TheVM* theVM();
  
  void startSmallSelf();

# else

  static const  int fixed_address_buffer_length = 128 * 1024 * 1024;

  static const int default_port = 9090;
  static const char initiation_request[]  =  "request_debug_server_1.5";
  static const char initiation_response[] =  "respond_debug_server_1.5";

  void runAllTests() {}

  void setBootstrapInfo(int, int) {}
  
  void startSmallSelf() {}

# endif

# endif // KLEIN_SMALL_VM_DECLARATIONS_OR_STUBS_H

    