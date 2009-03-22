/* 
$Revision: 1.1 $

A proxy server for debugging.

This server serves requests from remote clients
that specify debugging calls to the servers local OS environment.

Addressing:          server host IP, well-known port (see # define below)
Transport:           TCP sockets
Transmission format: proprietary


*/

# include "kleinDebugServer.hh"

# if defined(__APPLE__)
  # include "machDebugServer.hh"
# elif define(__linux__)
  # include "linuxDebugServer.hh"
# endif

# include "smallSelfDebugServer.hh"


const char* RequestTypes[] = {
  "zero",
  "terminate",
  "ptrace",
  "ping",
  "miniping",
  "waitStatus",
  "signal",
  "getReturnHandler",
  "getBaseAndLength",
  "setVMOop"
//  "startInterpreter"
};

const char* string_for_request_type(request_type_t t) {
  return (unsigned int)t  < sizeof(RequestTypes)/sizeof(RequestTypes[0])   ?  RequestTypes[t]  :  NULL;
}


// Debug flags:
//  Printing:
bool verbose             = false,
     print_request_types = false,
     print_children      = false;
    

// Debugging:
    

bool shift_base = false;
bool isClient    = false; // for testing
int  client_reps = 100;   // for testing


int BaseAndLengthGetter::fixed_address_buffer[fixed_address_buffer_length / sizeof(int)];

void breakpoint() {
   //since the debug server spawns child processes to do its work,
   //setting breakpoings inside of it is generally speaking not useful -> this 
   //breakpoint dumps out the pid of the child process. Use gdb to attach to it and debug.
   // - Ausch & Ungar, Nov/04
   printf_and_flush("at breakpoint");
   while(1);
 }


class ServerSocketOpener {
 public:
  void open_on_port(int p) {
    port = p;
    open_socket();
    make_socket_reusable();
    bind_socket();
    listen_for_connection();
  }
  
  int serverSocketFd() { return fd; }
    
 private:
  struct sockaddr_in serverAddress;
  int fd;
  int port;

  void open_socket() {
    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0)  { perror("socket() failed"); exit(1); }
    if (verbose) printf_and_flush("opened server socket\n");
  }
  
  // Avoids the problem of not being able to reopen socket right away
  // after a crash. Original by Jake, revised by dmu, 12/03.
  void make_socket_reusable() {
    int opt_val = true;
    if ( setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (void *)&opt_val, sizeof(opt_val)) < 0) {
        perror("SERVER WARNING: Could not make socket reusable.\n");
     }
     else if (verbose) printf_and_flush("make socket reusable\n");
  }
  
  void bind_socket() {
     bzero(&serverAddress, sizeof(serverAddress));
     serverAddress.sin_family = AF_INET;
     serverAddress.sin_port = htons(port);
     serverAddress.sin_addr.s_addr = htonl(INADDR_ANY);
	 
     if (bind(fd, (struct sockaddr *) &serverAddress, sizeof(serverAddress)) < 0) {
        char buf[1024];
        sprintf(buf, "SERVER ERROR: Could not bind server socket.\n"
                     "Is port %d already in use?\n", port);
        perror(buf);
        exit(1);
       
        if (verbose) printf_and_flush("bound socket\n");
     }
  }
  
  void listen_for_connection() {
    if (listen(fd, /*backlog*/ 100) < 0) {
        perror("SERVER ERROR: listen() failed");
        exit(2);  
    }
   if (verbose) printf_and_flush("listening on port %d\n", port);
  }
};


class ClientSocketOpener {

 public:
  int open_on_port(const char* hostName, int port) {
    get_host_entry(hostName);
    open_socket();
    connect_socket(port);
    return socket_fd;
  }
 private:
  struct hostent* host_entry;
  int socket_fd;
  
  void get_host_entry(const char* hostName) {
    host_entry = gethostbyname(hostName);
    if (host_entry == NULL) {
      perror("gethostbyname");
      error_printf_and_flush( "get_host_entry: gethostbyname failed host %s, h_errno %d",
              hostName, h_errno);
      exit(1);
    }
  }

  void open_socket() {
    socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (socket_fd == -1) {
      perror("socket");
      error_printf_and_flush( "open_socket: " "socket failed, errno = %d", errno);
      exit(1);
    }
  }

  void connect_socket(int port) {
    struct sockaddr_in a;
    a.sin_family = htons(AF_INET);
    a.sin_port = htons(port);
    long aLong;
    memcpy((char*)&aLong, host_entry->h_addr_list[0], sizeof(aLong));
    memset(a.sin_zero, 0, sizeof(a.sin_zero));
    a.sin_addr.s_addr = htonl(aLong);
    int res = connect(socket_fd, (struct sockaddr*)&a, sizeof(a));
    if (res) {
      perror("connect");
      error_printf_and_flush( "connect_socket: " "connect failed: errno = %d", errno);
      exit(1);
    }
  }
};



class ConnectionAcceptor {
public:
  ConnectionAcceptor(int fd) {
    serverSocketFd = fd;
    clientSocketFd = -1;
  }

  // initiate connection; fork, return -1 if parent, clientSocketFd if child

  int accept_and_fork() {
    for (;;) {
      collect_zombies();
      accept_a_connection();
      if (clientSocketFd < 0)
        continue;
        
      int childPid = fork();
      if (childPid == 0)
        break; // is child
      else if (childPid < 0)
        perror("SERVER WARNING: service creation (fork) failed");
      else if (print_children)
        printf_and_flush("forked child, pid = %d\n", childPid);
      close(clientSocketFd);
    }
    /*child*/
    close(serverSocketFd);
    if (verbose  ||  print_children)
      printf_and_flush("child returning %d\n", clientSocketFd);
    setpgrp(0, getpid());
    return clientSocketFd;
 }
          
 private:
  int serverSocketFd;
  int clientSocketFd;

  static void collect_zombies() {
    int status;
    if (verbose) printf_and_flush("collecting zomebies\n");
    for (int i = 0;  i < 10;  ++i) {
       pid_t child_pid = waitpid(-1, &status, WNOHANG);
       if (!print_children)
         ;
       else if (child_pid == 0)
         ; // none
       else if (child_pid == -1)
         ; // error
       else
         printf_and_flush("collected child %d\n", child_pid);
    }
    if (verbose) perror("waitpid");
  }

  void accept_a_connection() {
    struct sockaddr clientAddress;
    socklen_t nAddressBytes = sizeof(clientAddress);
    if (verbose)   printf_and_flush("accepting\n");
    clientSocketFd = accept(serverSocketFd,  &clientAddress, &nAddressBytes);
         if (clientSocketFd < 0)  perror("SERVER WARNING: client connect failed"); 
    else if (verbose)             printf_and_flush("accepted connection\n");
  }
};


class ServerInitiator: public SocketUser {
 private:

  // For SPARC, was: static const char arch[] = "sparc";
  char*  arch;
  char   error_or_null[1000];

  void set_arch() {
    static struct utsname my_utsname;
    if ( uname(&my_utsname) ) {
      perror("set_arch: uname");
      exit(1);
    }
    arch = my_utsname.machine;
    if (0 == strcmp( arch, "Power Macintosh") )
      arch = "ppc";
    if (verbose) printf_and_flush("set_arch() setting arch to: %s\n", arch);
  }

  char* read_request(int& len) { return s.read_bytes(len, "receiving initiation request"); }    
  

 public:
   ServerInitiator(BufferedSocket& ss) : SocketUser(ss) {set_arch(); }
    /* 
      How initiation works:

      Client sends INITIATION_REQUEST as ('\0'-terminated) string.
      Server responds with INITIATION_RESPONSE plus architecture string and my pid.
    */
    
  void initiate() {
    if (verbose) printf_and_flush("initiating\n"); 
    int len = -1;
    char* req = read_request(len);
    validate_request(req, len);
    delete req;
    send_response();
    send_arch();
    send_pid();
    s.flush_output();

    if (error_or_null[0])  exit(1);
  }
      
    
 private:

  
  void validate_request(char* req, int len) {
    error_or_null[0] = '\0';
    
    if (len != (int)strlen(initiation_request)) 
      sprintf(error_or_null, "Client Error: invalid initiation request length: "
                              "should have been: %d, was %d",
                              strlen(initiation_request), len);

    else if (strncmp(req, initiation_request, len) != 0)
      sprintf(error_or_null, "Client Error: invalid initiation request: should have been: %s, was %s",
                              initiation_request, req);
                              
   if (strlen(error_or_null) >= sizeof(error_or_null))  error_printf_and_flush("oops");

   if (error_or_null[0])
      error_printf_and_flush( "validate_request: %s", error_or_null);
   else if ( verbose )
      printf_and_flush("accepted initiation request\n");
  }    
  void send_response() { s.write_string(error_or_null[0] ? error_or_null : initiation_response,  "sending initiation response"); }
  void send_arch()     { s.write_string(arch,  "sending arch"); }
  void send_pid()      { s.write_int(getpid(), "sending pid"); }
};



class ClientInitiator: public SocketUser {
public:
  ClientInitiator(BufferedSocket& ss) : SocketUser(ss) {}
  /*
  How initiation works:
         Client sends INITIATION_REQUEST as ('\0'-terminated) string.
   Server responds with INITIATION_RESPONSE plus architecture string and my pid.
   */
  void initiate() {
    if (verbose) printf_and_flush("initiating\n");
    send_request();
    s.flush_output();
    read_response();
    read_arch();
    read_pid();
  }

private:
  void send_request() {
    s.write_string(initiation_request, "sending initiation request");
  }
  void read_response() {
    char* resp = s.read_string("getting initiation response");
    printf_and_flush("resp = %s\n", resp);
    delete resp;
  }
  void read_arch() {
    char* arch = s.read_string("getting arch");
    printf_and_flush("arch = %s\n", arch);
    delete arch;
  }
  void read_pid()      { s.read_int("getting pid"); }
};




class RequestServer: public SocketUser {
public:
  RequestServer(BufferedSocket& ss) : SocketUser(ss) {}
    
  bool serve_requests() {
      // return false when no more to serve
      read_request_type();
      if (verbose) printf_and_flush("servicing request type: %d\n", request_type);
      if (print_request_types && string_for_request_type(request_type))
        printf_and_flush("request type: %s  pid: %d...\n",
         string_for_request_type(request_type), getpid());

      bool r = false;
      switch (request_type) {
        case request_terminate:                                                                              break;
        case request_ptrace:                        { PTracer                x(s);  x.do_it(); }  r = true;  break;
        case request_ping:                          { Pinger                 x(s);  x.do_it(); }  r = true;  break;
        case request_miniping:                      { MiniPinger             x(s);  x.do_it(); }  r = true;  break;
        case request_waitStatus:                    { Waiter                 x(s);  x.do_it(); }  r = true;  break;
        case request_signal:                        { Signaller              x(s);  x.do_it(); }  r = true;  break;
        case request_getReturnHandler:              { ReturnHandlerGetter    x(s);  x.do_it(); }  r = true;  break;
        case request_getBaseAndLength:              { BaseAndLengthGetter    x(s);  x.do_it(); }  r = true;  break;
        
        case request_mach:                          { MachRequestServer      x(s);  r = x.do_it(); break; }
        case request_linux:		            { LinuxRequestServer     x(s);  r = x.do_it(); break; }
        case request_smallSelf:		            { SmallSelfRequestServer x(s);  r = x.do_it(); break; }

        
        default:  
        error_printf_and_flush( "serve_requests: " "bad request type: %d,  pid: %d", request_type, getpid()); 
        exit(1);
        return false; /* for compiler */
      }
      s.flush_output();
      if (print_request_types && string_for_request_type(request_type))
       printf_and_flush("request type: %s, pid: %d done\n",
        string_for_request_type(request_type), getpid());

      return r;
    }

 private:
  request_type_t request_type;

  void read_request_type() { request_type = (request_type_t)s.read_byte("reading request type"); }
};




class RequestSender: public SocketUser {
public:
  RequestSender(BufferedSocket& ss) : SocketUser(ss) {}
  void send_requests() {
    for (int i = 0; i < client_reps; ++i) {
      send_ping(); if (verbose) printf_and_flush("%d\n", i);
    }
  }
 private:
  void send_ping() {
    send_request_type(request_ping);     if (verbose)  printf_and_flush("sent rt\n");
    s.write_int(17, "fake pid");         if (verbose)  printf_and_flush("sent pid\n");
    s.flush_output();
    s.read_int("reading result");        if (verbose)  printf_and_flush("read res\n");
    char* err = s.read_string("error");  if (verbose)  printf_and_flush("read err\n");
    if (err[0]) {
      error_printf_and_flush( "ping failed: %s", err);
      exit(1);
    }
    delete err;
  }
  void send_miniping() {
    send_request_type(request_miniping);   if (verbose) printf_and_flush("sent rt\n");
    s.flush_output();
    s.read_int("reading result");       if (verbose)  printf_and_flush("read res\n");
  }
  void send_request_type(request_type_t rt) { s.write_byte(rt, "sending request type"); }
};


void error_printf_and_flush( char* msg, ...) {
  // Darned child process printf_and_flush doesn't work without the flush. -- dmu, jp 12/03
  va_list ap;
  va_start(ap, msg);
  fprintf(stderr, "kleinDebugServer[%d]: ", getpid());
  vfprintf(stderr, msg, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  fflush(stderr);
}


void printf_and_flush( char* msg, ...) {
  // Darned child process printf_and_flush doesn't work without the flush. -- dmu, jp 12/03
  va_list ap;
  va_start(ap, msg);
  printf ("[%d]: ", getpid());
  vprintf(msg, ap);
  va_end(ap);
  fflush(stdout);
}


void process_arguments(int argc, char *argv[], int& serverPort) {
  char* commandName = argv[0];
  for (;;) {
    int nargs = argc;
    if (argc > 1   &&  strcmp(argv[1], "-v") == 0) {
      verbose = true;
      --argc, ++argv;
    }
    if (argc > 1   &&  strcmp(argv[1], "-printRequestTypes") == 0) {
      print_request_types = true;
      --argc, ++argv;
    }
    if (argc > 1   &&  strcmp(argv[1], "-printChildren") == 0) {
      print_children = true;
      --argc, ++argv;
    }
    if (argc > 1   &&  strcmp(argv[1], "-printBufferLength") == 0) {
      printf_and_flush( "%d\n", fixed_address_buffer_length );
      --argc, ++argv;
    }
    if (argc > 1   &&  strcmp(argv[1], "-shiftBase") == 0) {
      shift_base = true;
      --argc, ++argv;
    }
    if (argc > 1   &&  strcmp(argv[1], "-c") == 0) {
      isClient = true;
      --argc, ++argv;
      if (argc > 1  &&  isdigit(argv[1][0])) {
        client_reps = atoi(argv[1]);
        --argc, ++argv;
      }
    }
    if (argc > 2  &&  strcmp(argv[1], "-p") == 0) {
      serverPort = atoi(argv[2]);
      argc -= 2;  argv += 2;
      if (verbose) printf_and_flush("setting serverPort to %d\n", serverPort);
    }
    if (argc > 1   &&  strcmp(argv[1], "-runAllTests") == 0) {
      runAllTests();
      --argc, ++argv;
    }
    if (nargs == argc)
      break;
  }
  if (argc > 1) {
    printf_and_flush("usage: %s [-c [client_reps]] [-v] [-printRequestTypes] [-printChildren] [-printBufferLength] [-runAllTests] "
                     "[-p portNum]\n",
                     commandName);
    exit(1);
  }
}


char* build_errno_status(int e, char* why) {
  if (e == 0) {
    char* empty = new char[1];
    empty[0] = '\0';
    return empty;
  }
  int whylen = strlen(why);
  char sep[] = ": ";
  int seplen = strlen(sep);
  char* err = strerror(e);
  int errlen = strlen(err);
  char* buf = new char[whylen + seplen + errlen + 1];
  strcpy(buf, why);
  strcpy(buf + whylen,  sep);
  strcpy(buf + whylen + seplen, err);
  return buf;
}

int doServer(int serverPort) {
  ServerSocketOpener so;
  so.open_on_port(serverPort);
  ConnectionAcceptor ca(so.serverSocketFd());
  int clientSocketFd = ca.accept_and_fork();
  // At this point, I am the child process
  BufferedSocket s(clientSocketFd);
  ServerInitiator in(s);
  in.initiate();
  RequestServer rs(s);
  while (rs.serve_requests()) {}
  return 0;
}


int doClient(int serverPort) {
  // for performance testing; a simple C client
  ClientSocketOpener co;
  printf_and_flush("opening client\n");
  int fd = co.open_on_port("localhost", serverPort);
  BufferedSocket s(fd);
  printf_and_flush("initiating client\n");
  ClientInitiator ci(s); 
  ci.initiate();
  RequestSender rs(s);
  printf_and_flush("sending requests\n");
  rs.send_requests();
  return 0;
}


void check_stdios() {
  for (int fd = 0;  fd <= 2;  ++fd) {
    int r = fcntl(fd, F_GETFL, 0);
    if (r == -1)  fprintf(stderr, "Warning: fcntl failed, fd: %d\n", fd);
    else if (r & (FASYNC | FNONBLOCK))  
      fprintf(stderr, "Warning: fd %d is in async or nonblocking mode.\nThis may interfere with running Klein.\n"
                      "Run kleinDebugServer in its own terminal window to fix this.\n", fd);
  }                      
}



int main(int argc, char* argv[]) {
    check_stdios();
  
    int serverPort = default_port;
    process_arguments(argc, argv, serverPort);

    return isClient ? doClient(serverPort) : doServer(serverPort);
}

