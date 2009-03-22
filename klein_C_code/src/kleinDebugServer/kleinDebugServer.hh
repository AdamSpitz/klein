# ifndef KLEIN_DEBUG_SERVER_HH
# define KLEIN_DEBUG_SERVER_HH

// $Revision: 1.1 $

# include <sys/types.h>
# include <sys/socket.h>
# include <sys/wait.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <unistd.h>
# include <fcntl.h>
# include <sys/ptrace.h>

// removed for Tiger (dmu, 5/05):
// # include <machine/ptrace.h>


# include <string.h>
# include <signal.h>
# include <netdb.h>
# include <sys/utsname.h>

# include "smallVMDeclarationsOrStubs.hh"
# include "utils.hh"

// required for Linux, innocuous for OSX -- Michael Furman 5/05
# include <ctype.h>
 
# include <stdarg.h>

# ifndef __APPLE__
  // Apparently needed for SOLARIS/SPARC and Linux/Mercury -- dmu 8/05
  typedef size_t nAddressBytes_t;
# endif

// Wrap for Linux;
int my_ptrace(int req, int pid, char* addr, int data);
    
// Debug flags:
//  Printing:
extern bool verbose, print_request_types, print_children;
    

// Debugging:
    

extern bool shift_base;
extern bool isClient; // for testing
extern int  client_reps;   // for testing


// the protocol: see platform specific header file

/* client sends ints big-endian, strings as int len followed by bytes (no null),
   initiates with initiation_request string,
   server responds with response string, then with arch string (either ppc or sparc for now)
   then client sends request type:
 */
 
enum request_type_t {
  request_terminate                  = 1,
  request_ptrace                     = 2,
  request_ping                       = 3,
  request_miniping                   = 4,
  request_waitStatus                 = 5,
  request_signal                     = 6,
  request_getReturnHandler           = 7,
  request_getBaseAndLength           = 8,

  request_mach                       = 100,
  request_linux                      = 101,
  request_smallSelf                  = 102
};

extern const char* RequestTypes[];

const char* string_for_request_type(request_type_t t);

void FlushCache(char* where, int len);

char* build_errno_status(int e, char* why);
void error_printf_and_flush( char* msg, ...);
void printf_and_flush( char* msg, ...);




class Socket {
 protected:
  int fd;
    
 public:
  Socket(int f) { fd = f; }
  Socket(const Socket& s) { fd = s.fd; }


  virtual void write_data(char* buf, int len, char* why) {
    if (verbose) printf_and_flush("writing data: %d bytes\n", len);
    int nb = 0;
    for (int total = 0;  total < len;  total += nb) {
      nb = write(fd, buf, len);
      if (nb <= 0) {
        perror(why);
        exit(1);
      }
    }
    if (verbose) printf_and_flush("write_data success: %s\n", why);
  }

  virtual int read_data(char* buf, int min_len, int max_len, char* why) {
    int nb = 0;
    int total = 0;
    for (total = 0;  total < min_len;  total += nb,  buf += nb) {
     nb = read(fd,  buf,  max_len - total);
      if (nb < 0) {
        perror(why);
        exit(1);
      }
      if (nb == 0) {
        error_printf_and_flush( "read_data: " "%s: EOF", why);
        exit(1);
      } 
    }
    if (verbose) printf_and_flush("read_data success: %s\n", why);
    return total;
  }
   
  void write_string(const char* the_string, char* why) {
    int len = strlen(the_string);
	write_int(len, why);
    write_data((char*)the_string, len, why);
    if (verbose) printf_and_flush("write_string success: %s\n", why);
  }
  
     
  char* read_string(char* why) {
    int len = read_int(why);
    char* buf = new char[len + 1];
    read_data(buf, len, len, why);
    buf[len] = '\0';
    if (verbose) printf_and_flush("read_string success: %s\n", why);
    return buf;
  }
   
  char* read_bytes(int& len, char* why) {
    len = read_int(why);
	char* buf = new char[len];
    read_data(buf, len, len, why);
    if (verbose) printf_and_flush("read_bytes success: %s\n", why);
    return buf;
  }
   
  void write_bytes(char* the_bytes, int len, char* why) {
    write_int(len, why);
    write_data(the_bytes, len, why);
    if (verbose) printf_and_flush("write_bytes success: %s\n", why);
  }
   
  void write_int(int i, char* why) {
    char buf[4];
    buf[0] = char(i >> 24);
    buf[1] = char(i >> 16);
    buf[2] = char(i >>  8);
    buf[3] = char(i);
    write_data(buf, sizeof(buf), why);
    if (verbose) printf_and_flush("write_int success: %s\n", why);
  }

  int read_int(char* why) {
    char buf[4];
	read_data(buf, sizeof(buf), sizeof(buf), why);
	
    int r = (buf[0] << 24) | ((buf[1] & 0xff) << 16) | ((buf[2] & 0xff) << 8) | (buf[3] & 0xff);
    if (verbose) printf_and_flush("read_int success: %d, %s\n", r, why);
    return r;
  }
   
  void write_byte(char  b, char* why) { write_data(&b, 1, why); }
  char  read_byte(         char* why) { char b;  read_data(&b, 1, 1, why);  return b; }
   
  void read_int_array(int*& int_array, int& length, char* why) {
    int_array = NULL;  length = 0;
    length = read_int(why);
    int_array = new int[length];
    for (int i = 0;  i < length;  ++i)
      int_array[i] = read_int(why);
  }
  
  void write_int_array(int* int_array, int length, char* why) {
    write_int(length, why);
    for (int i = 0;  i < length;  ++i)
      write_int(int_array[i], why);
  }

   
  void write_errno_status(int e, char* why) {
    char* status = build_errno_status(e, why);
    write_string(status, why);
    delete status; // Don't need "delete[]"; no recursive destruction -- dmu 8/05
  }
};

class FileHandler: public Socket {
 public:
  FileHandler(int f)                     : Socket(fd) {fd=f;};
  
  FileHandler(char* file_name, int open_flags) : Socket (fd) {
	fd = open (file_name, open_flags);
	if (verbose) printf_and_flush("creating file handler,   fd: %d\n",fd);
	if (fd < 0) error_printf_and_flush("Error while opening file: %s", file_name);
  };
  
  FileHandler(char* file_name, int open_flags, int permission_flags) : Socket(fd) {
	fd = open (file_name, open_flags, permission_flags);
	if (verbose) printf_and_flush("creating file handler and setting permission flags,   fd: %d\n",fd);

	if (fd < 0) error_printf_and_flush("Error while opening file: %s", file_name);
  };
  
  void close_file () {
	if (close(fd) < 0) error_printf_and_flush("Error while closing file");
  };
};

class BufferedSocket: public Socket {
 public:
  BufferedSocket(int f)           : Socket(f) {next_input = next_output = input_length = 0;}
  BufferedSocket(const Socket& s) : Socket(s) {next_input = next_output = input_length = 0;}


  void write_data(char* buf, int len, char* why) {
    if (verbose) printf_and_flush("BufferedSocket:write_data %d bytes\n", len);
    for (;;) {
      int this_time = min( sizeof(output_buffer) - next_output,  len );
      memcpy(output_buffer + next_output, buf, this_time);
      buf += this_time;
      next_output += this_time;
      len -= this_time;
      output_why = why;
      if (verbose) printf_and_flush("BufferedSocket::write_data wrote %d bytes\n", this_time);
      if (len == 0)
        return;
      flush_output();
    }
  }

  int read_data(char* buf, int min_len, int max_len, char* why) {
    int total = 0;
    for ( ; min_len > 0; ) {
      input_why = why;
      read_input();
      int this_time = min(max_len, input_length);
      memcpy(buf, input_buffer + next_input, this_time);
      buf += this_time;
      next_input += this_time;
      total += this_time;
      input_length -= this_time;
      min_len -= this_time;
      max_len -= this_time;
    }
    return total;
  }
 
  void flush_output() {
    if (verbose) printf_and_flush("flush_output %d bytes, %s\n", next_output, output_why);
    if (next_output)
      Socket::write_data(output_buffer, next_output, output_why);
    next_output = 0;
  }
private:
  static const int buf_len = 110000;
  char  input_buffer[buf_len];
  char  output_buffer[buf_len];
  int   next_input;
  int   input_length;
  int   next_output;
  char* input_why;
  char* output_why;

  void read_input() {
    if (input_length == 0) {
      input_length = Socket::read_data(input_buffer, 1, sizeof(input_buffer), input_why);
      next_input = 0;
    }
  }
};



class SocketUser {
  protected:
    BufferedSocket& s;
  public:
    SocketUser(BufferedSocket& ss) : s(ss) {}
};
 
 
class PTracer: public SocketUser {
 public:
  PTracer(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    int req  = s.read_int("getting request");
    int pid  = s.read_int("getting pid");
    int addr = s.read_int("getting address");
    int data = s.read_int("getting data");
    errno = 0;
    int res = my_ptrace(req, pid, (char*)addr, data);
    int e = errno;
    if (verbose) 
      printf_and_flush("PTracer(%d, %d, 0x%x, 0x%x) = %d, errno = %d\n",
             req, pid, addr, data, res, e);
    s.write_int(res, "writing ptrace result");
    s.write_errno_status(e, "ptrace");
  }
};


class Pinger: public SocketUser {
 public:
  Pinger(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    int pid  = s.read_int("getting pid");
    if (verbose) 
      printf_and_flush("ping(%d)\n", pid);
    s.write_int(pid, "result");
    s.write_string("", "no error");
  }
};


class MiniPinger: public SocketUser {
 public:
  MiniPinger(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    if (verbose) 
      printf_and_flush("miniping\n");
    s.write_int(17, "result");
  
  }
};


class Waiter: public SocketUser {
 public:
  Waiter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    int status = 0;
    int pid  = s.read_int("getting pid");
    pid_t result_pid = waitpid(pid, &status, WNOHANG | WUNTRACED);
    int e = result_pid == -1 ? errno : 0;

    if (verbose)  printf_and_flush("waitpid pid %d, result %d, status 0x%x, errno %d\n",
                         pid, result_pid, status, e);
    s.write_int(result_pid, "result");
         if (result_pid == -1)  s.write_errno_status(e, "waitpid");
    else if (result_pid !=  0)  s.write_int(status, "status");
  }
};


class Signaller: public SocketUser {
 public:
  Signaller(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    int pid  = s.read_int("getting pid");
    int sig  = s.read_int("getting signal");
    errno = 0;
    kill(pid, sig);
    int e = errno;
    s.write_errno_status(e, "kill");
  }
};


// used as LR value when running debuggee

static void return_handler_fn(int result) {
  printf_and_flush("debuggee returned, pid %d, result %d (0x%x)\n", getpid(), result, result);
  exit(0);
}


class ReturnHandlerGetter: public SocketUser {
 public:
  ReturnHandlerGetter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
     s.write_int((int)return_handler_fn, "kill");
  }
};


class BaseAndLengthGetter: public SocketUser {
  // increase constant below as needed
  static int fixed_address_buffer[];
 public:
  BaseAndLengthGetter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
     static const int shift = 1024;
     s.write_int((int)fixed_address_buffer   + (shift_base ? shift : 0), "buffer address");
     s.write_int(fixed_address_buffer_length - (shift_base ? shift : 0), "buffer length");
  }
};


// return largest r <= x that is a multiple of y
// y must be a power of two
inline unsigned int round_down(unsigned int x, unsigned int y) {
  return x & ~(y - 1);
}

// return smallest r >= x that is a multiple of y
// y must be a power of two
inline unsigned int round_up(unsigned int x, unsigned int y) {
  return (x + y - 1) & ~(y - 1);
}


class AddressInterval {
 private:
  unsigned int _bottom, _top; // top is first byte NOT in interval
 public:
   unsigned int bottom() const { return _bottom; }
   unsigned int top()    const { return _top; }
   unsigned int last()   const { return top() - 1; }
   unsigned int length() const { return top() - bottom(); }
   
   AddressInterval(unsigned int b, unsigned int s) { _bottom = b;  _top = b + s; }
   AddressInterval(const AddressInterval& ai) { _bottom = ai.bottom(); _top = ai.top(); }
   
   // round bottom down to even page boundary
   AddressInterval round_bottom(unsigned int page_size) const { 
     unsigned int b = round_down(bottom(), page_size);
     return AddressInterval(b, length() + (bottom() - b));
  }
  
  // round top up to even page boundary
  AddressInterval round_top(unsigned int page_size) const {
    unsigned int t = round_up(top(), page_size);
    return AddressInterval(bottom(), length() + (t - top()));
  }
  
  AddressInterval round(unsigned int page_size) const {
    return round_bottom(page_size).round_top(page_size);
  }
  
  AddressInterval trim_top_by(int delta) const {
    return AddressInterval(bottom(), length() - delta);
  }
  
  AddressInterval trim_bottom_by(int delta) const {
    return AddressInterval(bottom() + delta, length() - delta);
  }
  
  AddressInterval min(AddressInterval x) const {
    unsigned int b = ::max(bottom(), x.bottom());
    unsigned int t = ::min(top(),    x.top());
    return AddressInterval(b, t - b);
  }
  
  bool operator ==(const AddressInterval& x) const {
    return bottom() == x.bottom()  &&  length() == x.length();
  }
  
  bool operator !=(const AddressInterval& x) const {
    return !(*this == x);  }
};  

# endif // KLEIN_DEBUG_SERVER_HH
