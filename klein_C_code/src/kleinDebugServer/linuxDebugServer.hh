// This file is only for Linux (the Mercury board)
// $Revision: 1.1 $


# include <inttypes.h>
# include <stdio.h>
# include <netinet/in.h>
# include <fstream>
# include <sys/types.h>
# include <sys/stat.h>
# include <unistd.h>
# include <fcntl.h>
# include <linux/ptrace.h>
# include <asm/ptrace.h>


typedef uint32_t int_reg_t;


enum linux_request_type_t {
  linux_request_get_integer_state = 1,
  linux_request_set_integer_state = 2,

  linux_request_get_memory  = 3,
  linux_request_set_memory  = 4,
  linux_request_run_state   = 5,
  linux_request_multi_step  = 6,
};

const char* LinuxRequestTypes[] = {
  "zero",
  "get_ingeger_state",
  "set_integer_state",
  "get_memory",
  "set_memory",
  "get_run_state",
  "multi_step",
};


const char* string_for_linux_request_type(linux_request_type_t t) {
  return (unsigned int)t < sizeof(LinuxRequestTypes)/sizeof(LinuxRequestTypes[0])
          ?  LinuxRequestTypes[t]  
          :  NULL;
};


static bool step_taken = false;
void    set_step_taken() { step_taken = true;  }
void  reset_step_taken() { step_taken = false; }


int my_ptrace(int req, int pid, char* addr, int data) {

  int res = ::ptrace( __ptrace_request(req), pid, (char*)addr, data);

  if ( verbose )  {
    int e = errno;
    printf_and_flush("ptrace(%d, %d, 0x%x, 0x%x) = %d, errno = %d\n",
           req, pid, addr, data, res, e);
    errno = e;
  }

  // Set the step_taken_yet flag properly for Linux PC 
  // setting and getting. A step is considered to be
  // taken when a single step, continue, or detach ptrace command
  // is run. This flag is used in the following functions to 
  // get and set the current program counter properly:
  // LinuxInterface::set_pc() and LinuxInterface::get_pc()
  // Also see flowchart, step_taken_yet.png --Michael Furman 8/05
  switch( req ) {
   case PTRACE_CONT:
   case PTRACE_SINGLESTEP:
   case PTRACE_DETACH:
    set_step_taken();
  }

  return res;
}


// see linux manpage proc(5) and /usr/src/linux/fs/proc/array.c
class ProcessStatus {
    private:
                int pid;
               char *comm;
               char state;
                int ppid;
                int pgrp;
                int session;
                int tty_nr;
                int tpgid;
      unsigned long flags;
      unsigned long minflt;
      unsigned long cminflt;
      unsigned long majflt;
      unsigned long cmajflt;
      unsigned long utime;
      unsigned long stime;
               long cutime;
               long cstime;
               long priority;
               long nice;
               long zero;
               long itrealvalue;
      unsigned long starttime;
      unsigned long vsize;
               long rss;
      unsigned long rlim;
      unsigned long startcode;
      unsigned long endcode;
      unsigned long startstack;
      unsigned long kstkesp;
      unsigned long kstkeip;
      unsigned long signal;
      unsigned long blocked;
      unsigned long sigignore;
      unsigned long sigcatch;
      unsigned long wchan;
      unsigned long nswap;
      unsigned long cnswap;
                int exit_signal;
                int processor;
  public:
    ProcessStatus()                    { comm = NULL; }
    ProcessStatus(const char *buffer)  { get_pid_stat(buffer);  }

    ~ProcessStatus()                   { if (comm) delete comm; }


    char get_state() { return state; }

    void get_pid_stat(const char *inbuf) {
      char *end = strstr(inbuf,"\n");
      int len = end - inbuf;
      char *tbuf = new char[len+1];
      strncpy(tbuf, inbuf, len);
      sscanf(tbuf, "%d %s %s "
                    "%d %d %d %d %d %d "
                    "%d %d %d %d %d %d "
                    "%d %d %d %d %d %d "
                    "%d %d %d %d %d %d "
                    "%d %d %d %d %d %d "
                    "%d %d %d %d %d %d",
                     &pid,          &comm,      &state,
                     &ppid,         &pgrp,      &session,    &tty_nr,     &tpgid,        &flags,       
                     &minflt,       &cminflt,   &majflt,     &cmajflt,    &utime,        &stime,        
                     &cutime,       &cstime,    &priority,   &nice,       &zero,         &itrealvalue,  
                     &starttime,    &vsize,     &rss,        &rlim,       &startcode,    &endcode,      
                     &startstack,   &kstkesp,   &kstkeip,    &signal,     &blocked,      &sigignore,    
                     &sigcatch,     &wchan,     &nswap,      &cnswap,     &exit_signal,  &processor);

      if ( verbose ) fprintf(stderr, "pid %d cmd %s state %c ppid %d\n",pid,comm, state, ppid);
      // there might be conversion errors.  Right now we really don't care.
      // go ahead and clobber errno and everything will be happy. -- Michael Furman 8/05
      errno=0;
      delete tbuf;`:w
    }


    // Rewrote as above; delete me when above works. -- dmu 9/05
    void old_get_pid_stat(const char *inbuf) {
      char *end = strstr(inbuf,"\n");
      int len = end - inbuf;
      char *tbuf = new char[len+1];
      strncpy(tbuf, inbuf, len);
      tbuf[len]='\0';
      char *t;


      t = strtok(tbuf, " "); pid = atoi(t);
      t = strtok(NULL, " "); comm = new char[strlen(t) + 1];
      strncpy(comm, t, strlen(t) +1);
      t = strtok(NULL, " "); state = *t;
      strncpy(state, t, strlen(t) +1);

      # define getit(var_name)  t = strtok(NULL, " ");  var_name = atoi(t)

        getit( ppid        );   getit( pgrp        );   getit( session     );
        getit( tty_nr      );   getit( tpgid       );   getit( flags       );
        getit( minflt      );   getit( cminflt     );   getit( majflt      );   
        getit( cmajflt     );   getit( utime       );   getit( stime       );   
        getit( cutime      );   getit( cstime      );   getit( priority    );   
        getit( nice        );   getit( zero        );   getit( itrealvalue );   
        getit( starttime   );   getit( vsize       );   getit( rss         );   
        getit( rlim        );   getit( startcode   );   getit( endcode     );   
        getit( startstack  );   getit( kstkesp     );   getit( kstkeip     );   
        getit( signal      );   getit( blocked     );   getit( sigignore   );   
        getit( sigcatch    );   getit( wchan       );   getit( nswap       );   
        getit( cnswap      );   getit( exit_signal );   getit( processor   );   

     # undef getit

      if ( verbose ) fprintf(stderr, "pid %d cmd %s state %c ppid %d\n",pid,comm, state, ppid);
      // there might be conversion errors.  Right now we really don't care.
      // go ahead and clobber errno and everything will be happy. -- Michael Furman 8/05
      errno=0;
      delete tbuf;
    }

}; // class ProcessStatus


class LinuxInterface: public SocketUser {
  public:
    LinuxInterface(BufferedSocket &ss): SocketUser(ss) {}

  protected:
    // caller must free buffer later
    void get_memory(int pid,  AddressInterval requested,  char*&  buffer,  unsigned int& length) {
      errno = 0;
      length = 0;

      if (pid == 0) { errno = EPERM;  return; } // Never attempt to debug the parent process

      buffer = new char[requested.length()]; // create space for result. caller must delete it.

      char p[256];
      sprintf(p, "/proc/%d/mem", pid);
      if ( !existsProcFile(p) )
        return;
      int mfp = open (p,  O_RDONLY);
      if ( mfp == -1)
        return;
      if ( !seekProcFile(mfp,  requested.bottom()) )
        return;

      const int page_size = 4096;

      char readbuf[page_size];
      int countread = 0;
      // we read up to page_size at one time. loop until we get everything.
      while ( countread < requested.length()) {
        int readlen = requested.length() - countread;
        if ( readlen > page_size )
          readlen = page_size;
        int rv = read(mfp,  readbuf,  readlen);
        if ( rv == -1 )
          return;
        memcpy(buffer + countread,  readbuf,  rv);
        countread += rv;
      }

      close (mfp);
      length = countread;
      return;
    } 

    void set_memory(int pid, char*& buffer, AddressInterval requested) {
      errno = 0;
      // Never attempt to debug the parent process
      if (pid == 0) { errno=EPERM; return; }

      char p[256];
      sprintf(p,  "/proc/%d/mem",  pid);
      if ( ! existsProcFile(p) )
        return;
      int mfp= open(p,  O_RDWR);
      if ( mfp == -1 )
        return;
      if ( ! seekProcFile(mfp,  requested.bottom()) )
        return;

      int countwritten = 0;
      // loop until we have written everything.
      while ( countwritten < requested.length() ) {
        int rv = write(mfp,  buffer + countwritten,  requested.length());
        if ( rv == -1 )
          return;
        countwritten += rv;
      }

      close (mfp);
      return;
    }


    void examineDataNearAddr (int pid,  int addr) {
      for( int i = -sizeof(int_reg_t);  i < 8;  i += sizeof(int_reg_t)  )  {
        int result = my_ptrace (PTRACE_PEEKDATA,  pid,  addr + i,  0);
        printf_and_flush(" D 0x%x: 0x%x\n",  addr + i,  result);
      }
    }

    int get_msr (int pid) {
      errno = 0;
      int rv = my_ptrace(PTRACE_PEEKUSR,  pid,  PT_MSR * sizeof(int_reg_t),  0);
      return rv;
    }


    int get_pc (int pid) {
      errno = 0;
      int next_instruction_pointer = my_ptrace(PTRACE_PEEKUSR,  pid,  PT_NIP * sizeof(int_reg_t),  0);
      if ( errno ) return -1;

      int msr = get_msr(pid);
      if ( errno ) { perror("could not get msr"); return -1; }

      if ( verbose ) printf_and_flush( "GetPC: Retreived PT_NIP=0x%x PT_MSR=%x\n", next_instruction_pointer, msr );

      // If we haven't taken a step into the program after setting the PC,
      // the pc needs to be adjusted down by  one instruction in order to
      // show the next instruction to be executed.  This is done for 
      // consistency on the Klein debugger screen -- Michael Furman 8/05
      // (Linux weirdness; even gdb does j* wrong. -- dmu & Mikey Furman.)

      if ( !step_taken ) next_instruction_pointer -= sizeof(int_reg_t);
      
      if ( verbose ) printf_and_flush( "GetPC: Corrected PT_NIP=0x%x PT_MSR=%x\n", next_instruction_pointer, msr );

      return next_instruction_pointer;
    }


    int set_pc (int pid,  int value) {
      errno = 0;

      if ( verbose ) printf_and_flush(  "SetPC: Requested PT_NIP=0x%x\n",  value);

      // we need to bump the PC up by 1 instruction otherwise the wrong
      // instruction will be executed, or worse will get SIGILL -- Michael Furman 7/05
      value+=4;

      if ( verbose ) printf_and_flush( "SetPC: Corrected PT_NIP=0x%x\n",  value );

      int rv = my_ptrace( PTRACE_POKEUSR,  pid,  PT_NIP * sizeof(int_reg_t),  value);

      reset_step_taken();

      return value; // Returns value of corrected PC for convenience.
                    // This breaks from the typical return value of a ptrace
                    // call, which should be 0 if success -- Michael Furman 8/05
    }

    int get_integer_registers(int pid,  int *&registerBuffer) {
      const int num_int_regs = PT_FPR0;
      registerBuffer = new int[num_int_regs];
      errno=0;

      for( int index = 0;  index < num_int_regs  &&  errno == 0;  ++index ) {
        if ( index == PT_NIP )
          registerBuffer[index] = get_pc(pid);
        else
          registerBuffer[index] = my_ptrace( PTRACE_PEEKUSR, pid,  index * sizeof(int_reg_t),  0);
      }
      if ( errno ) return 0;

      if ( verbose ) examineDataNearAddr (pid, registerBuffer[PT_NIP]);

      return num_int_regs;
    } // get_integer_registers


    bool set_integer_registers(int pid, int *&registerBuffer, int len) {
      int pc;
      errno = 0;
      
      for( int index = 0;   index < len && errno == 0 ;  index++ ) {
        if ( ! isRegWritable(index) )
          continue;
        if ( index == PT_NIP )
          pc = set_pc ( pid,  registerBuffer[index] );
        else
          int result = my_ptrace( PTRACE_POKEUSR,  pid,  index * sizeof(int_reg_t),  registerBuffer[index]);
      }

      if ( errno ) return false;
      if ( verbose ) examineDataNearAddr(pid, pc);

      return true;
    } // set_integer_registers


    char get_run_state(int pid) {
      errno = 0;

      char p[256];
      sprintf( p,  "/proc/%d/stat",  pid);
      if ( ! existsProcFile(p) )
        return 0;

      int mfp = open ( p,  O_RDONLY);
      if ( mfp == -1 )
        return 0;

      const int buf_size = 256;
      char buf[buf_size];
      int rv = read( mfp,  buf,  buf_size);
      if( rv == -1 ) return 0;
      close(mfp);

      ProcessStatus ps(buf);
      char c = ps.get_state();

      return c;
    }


    bool multi_step(int pid, int addr, int maxSteps, int targetAddr) {
      // todo linuxDebugServer this is not tested yet -- Michael  Furman 8/05
      int e, pc, res;
      int last_pc = -1;
      if ( verbose ) printf_and_flush("looking for addr during multi-step: 0x%x\n",targetAddr);
      if ( verbose ) printf_and_flush("addr: 0x%x\n",addr);
      pc = set_pc(pid, addr);
      if ( errno )
        return false;
      pc = get_pc(pid);
      if( errno )
        return false;
      if ( verbose ) printf_and_flush("PC before multi-step: 0x%x\n",pc);
      for ( int nSteps = 0;  nSteps < maxSteps;  ++nSteps ) {
        pc = get_pc(pid);
        if ( verbose ) printf_and_flush("pc is now: 0x%x\n",pc);

        if ( pc == 0  ||  pc == targetAddr || pc == last_pc )
          break;
        res=my_ptrace(PTRACE_SINGLESTEP,  pid,  1,  0);
        if ( errno )
          return false;
        addr = 1; // continue from where you are thereafter
        last_pc = pc;
      }
      return true;
    }

  private:
    bool existsProcFile (char * p) {
      struct stat pidstat;
      int rv = stat( p,  &pidstat);
      if ( rv )
        return false;
      else
        return true;
    }


    bool seekProcFile (int fp, int pos) {
      int rv = lseek( fp,  pos,  SEEK_SET);
      if ( rv != pos )
        return false;
      return true;
    }


    bool isRegWritable (int registerIndex) {
      // see <asm/ptrace.h> for details
      // Why is this here?  A careful look at the kernel source
      // code arch/ppc/kernel/ptrace.c shows that up to 48 
      // registers are allowed for in the user structure.
      // The registers listed here can be read but not written to.
      // Known Errors: - writing to PT_ORIG_R3 returns EIO.
      //               - Indexes 40 - 47 in pt_regs are not defined in <asm/ptrace.h>
      // --Michael Furman 7/05

      bool rv=true;
      # define PT_ORIG_R3 34 // from <asm/ptrace.h>
      switch (registerIndex) {
        case PT_ORIG_R3:
        case 40:
        case 41:
        case 42:
        case 43:
        case 44:
        case 45:
        case 46:
        case 47:
          rv=false;
          break;
        default:
          break;
      }
      return rv;
    } //isRegWritable
};


class MemoryGetter: public LinuxInterface {
  public:
    MemoryGetter(BufferedSocket &ss): LinuxInterface(ss) {};

    void do_it() {
      int pid    = s.read_int("getting pid");
      unsigned int addr   = s.read_int("getting addr");
      unsigned int len    = s.read_int("getting length");
      char* buf_to_free   = NULL;
      unsigned int length_to_free = 0;

      get_memory(pid,  AddressInterval(addr,  len),  buf_to_free,  length_to_free);
      int e = errno;
      s.write_bytes(buf_to_free, length_to_free, "writing memory");
      s.write_errno_status(e, "get_memory");

      delete []buf_to_free;
    }
};


class MemorySetter: public LinuxInterface {
  public:
    MemorySetter(BufferedSocket& ss) : LinuxInterface(ss) {}

    void do_it() {
      int pid    = s.read_int("getting pid");
      int len = 0;
      char* buf = s.read_bytes( len,  "reading memory contents");
      int addr = s.read_int("getting address");

      set_memory(pid,  buf,  AddressInterval(addr,  len));
      s.write_errno_status( errno ,  "set_memory");

      delete buf;
    }
};



// gets registers by doing multiple ptrace calls
class IntegerStateGetter: public LinuxInterface {
  public:
    IntegerStateGetter(BufferedSocket &ss):  LinuxInterface(ss) {};

    void do_it() {
      int pid = s.read_int("geting pid");
      int *registers = NULL;

      int nregs = get_integer_registers(pid,  registers);
      int e = errno;
      s.write_errno_status(e,  "get_integer_state");
      if (!e)
        s.write_int_array(registers,  nregs,  "writing registers");

      delete registers;
    }
};

// sets registers by doing multiple ptrace calls
class IntegerStateSetter: public LinuxInterface {
  public:
    IntegerStateSetter(BufferedSocket &ss):  LinuxInterface(ss) {};

    void do_it() {
      int pid = s.read_int("geting pid");
      int *registers;
      int len;
      s.read_int_array(registers, len, "getting registers");

      set_integer_registers(pid, registers, len);
      s.write_errno_status(errno, "set_integer_registers");

      delete registers;
    }
};



class RunStateGetter: public LinuxInterface {
  public:
    RunStateGetter(BufferedSocket &ss): LinuxInterface(ss) {};
    void do_it() {
      int pid = s.read_int("getting pid");
      char c = get_run_state(pid);
      int e = errno;
      write(1, &c, 1);
      s.write_byte(c, "writing run_state");
      s.write_errno_status(e, "get_run_state");
    }

};

class MultiStepper: public LinuxInterface {
 public:
  MultiStepper(BufferedSocket& ss) : LinuxInterface(ss) {}
  void do_it() {
    int pid  = s.read_int("getting pid");
    int addr       = s.read_int("getting continuation addr");
    int maxSteps   = s.read_int("getting max steps");
    int targetAddr = s.read_int("getting target addr");
    int e=multi_step(pid, addr, maxSteps, targetAddr);
    s.write_errno_status(e, "multi_step");
  }
};

class LinuxRequestServer: public SocketUser {
  public:
    LinuxRequestServer(BufferedSocket &ss): SocketUser(ss) {};

    bool do_it() {
      linux_request_type_t request_type = (linux_request_type_t)s.read_byte("reading linux request type");
      if ( verbose ) printf_and_flush("servicing linux request type: %d\n", request_type);

      if (print_request_types  &&  string_for_linux_request_type(request_type))
        printf_and_flush("request type: %s (linux), pid: %d...\n",
      string_for_linux_request_type(request_type), getpid());

      bool r = true;
      switch (request_type) {
        case linux_request_get_integer_state:     { IntegerStateGetter   x(s);  x.do_it(); }  break;
        case linux_request_set_integer_state:     { IntegerStateSetter   x(s);  x.do_it(); }  break;
        case linux_request_get_memory:            { MemoryGetter         x(s);  x.do_it(); }  break;
        case linux_request_set_memory:            { MemorySetter         x(s);  x.do_it(); }  break;
        case linux_request_run_state:             { RunStateGetter       x(s);  x.do_it(); }  break;
        case linux_request_multi_step:            { MultiStepper         x(s);  x.do_it(); }  break;

        default:  error_printf_and_flush( "LinuxRequestServer: " "bad linux request type: %d", request_type);  exit(1);  return false;
      }

      if (print_request_types  &&  string_for_linux_request_type(request_type))
        printf_and_flush("request type: %s (linux), pid %d done\n",
      string_for_linux_request_type(request_type), getpid());
      return r;
    }
};
