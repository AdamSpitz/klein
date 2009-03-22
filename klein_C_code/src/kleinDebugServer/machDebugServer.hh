// This file is only included for OS X; it does Mach-level operations
// $Revision: 1.1 $
 

# include <inttypes.h>
# include <stdio.h>
# include <netinet/in.h>
# include <CoreServices/CoreServices.h>
# undef assert
# include <mach/mach_init.h>
# include <mach/task.h> // jaguar
# include <mach/vm_map.h> // jaguar
# include <mach/thread_act.h> // jaguar
# include <mach/mach_traps.h>
# include <mach/mach_error.h>
# include <mach/ppc/thread_status.h>


typedef int nAddressBytes_t;

void FlushCache(char* where, int len) {
  if (verbose)  printf_and_flush("Flushing cache at: 0x%x length: %d\n", where, len);
  //MakeDataExecutable(where, len);
}

// Linux needs a wrapper
int my_ptrace(int req, int pid, char* addr, int data) { return ptrace(req, pid, addr, data); }

enum mach_request_type_t {
  mach_request_task_for_pid          =  1,
  mach_request_threads_for_task      =  2,
  mach_request_get_integer_state     =  3,
  mach_request_set_integer_state     =  4,
  mach_request_get_memory            =  5,
  mach_request_set_memory            =  6,
  mach_request_run_state             =  7,
  mach_request_single_step           =  8,
  mach_request_multi_step            =  9,
  mach_request_allocate_memory       = 10,
  mach_request_write_memory_to_file  = 11,
  mach_request_read_memory_from_file = 12,
  mach_request_remove_file           = 13,
  mach_request_download_image        = 14
};

const char* MachRequestTypes[] = {
  "zero",
  "task_for_pid",
  "threads_for_task",
  "get_integer_state",
  "set_integer_state",
  "get_memory",
  "set_memory",
  "run_state",
  "single_step",
  "multi_step",
  "allocate_memory",
  "write_memory_to_file",
  "read_memory_from_file",
  "remove_file",
  "download_image"
};

const char* string_for_mach_request_type(mach_request_type_t t) {
  return (unsigned int)t < sizeof(MachRequestTypes)/sizeof(MachRequestTypes[0])  ?  MachRequestTypes[t]  :  NULL;
}

class MemoryManipulator {
 public:
   MemoryManipulator() {
    error_string[0] = '\0';
    mach_error_string[0] = '\0';
  }
   
 protected:
   task_t get_task_for_pid(int pid) {
     task_t r;    
     if ( check_kern_call(task_for_pid(mach_task_self(), pid, &r))) {
       return  r;
     }
     sprintf(error_string, "%s, while trying to locate task for pid %d. Your userid needs to belong to the \"procmod\" group",
               mach_error_string, pid);
     return 0;
   }
   
   // caller must call free_thread_array on array afterwards
  bool get_task_threads(task_t task, thread_array_t& thread_list, unsigned int& thread_count) {
    if ( !check_kern_call( ::task_threads( task, &thread_list, &thread_count) )) {
      thread_list = NULL;  thread_count = 0;
      sprintf(error_string, "%s, while trying to get task %d threads", mach_error_string, task);
      return false;
    }
    return true;
  }

  bool free_thread_array(thread_array_t thread_list, unsigned int thread_count) {
    if (thread_list == NULL)
      return true;
    if (check_kern_call( vm_deallocate(mach_task_self(),  (vm_address_t)thread_list,  
                                       sizeof(thread_list) * thread_count)))
      return true;
    sprintf(error_string, "%s, while trying to free thread array", mach_error_string);
    return false;
  }
  
  void print_thread_array(thread_array_t thread_list, unsigned int thread_count) {
    for (unsigned int i = 0;  i < thread_count;  i++) {
      printf_and_flush("thread: %d\n", (int) thread_list[i]);
    }
  }
  
  thread_t get_thread_for_task(task_t t) {
    thread_array_t threads;  unsigned int nthreads;
    if (!get_task_threads(t, threads, nthreads))
      return 0;
    if (nthreads == 0) {
      sprintf(error_string, "no threads!");
      return 0;
    }
    if (nthreads > 1) {
      sprintf(error_string, "too many threads (%d)", nthreads);
      return 0;
    }
    thread_t r = threads[0];
    free_thread_array(threads, nthreads);
    return r;
  }
  
  thread_t get_thread_for_pid(int pid) {
    task_t t = get_task_for_pid(pid);
    if (t == 0)  return t;
    return get_thread_for_task(t);
  }
  

  int get_pc(int pid) {
    thread_state_data_t regs;
    int nregs = get_thread_state(pid, MACHINE_THREAD_STATE, regs);
    if (nregs == 0)
      return 0;
    # ifdef __ppc__
      return ((struct ppc_thread_state*)regs)->srr0;
    # elif defined(__i386__)
      return ((struct i386_thread_state*)regs)->eip;
    # else
      #error what arch?
    # endif
  }
  
  
  int get_thread_state(int pid, int flavor, thread_state_data_t regs) {
    // flavor is MACHINE_THREAD_STATE, etc.
    thread_t t = get_thread_for_pid(pid);
    if (t == 0)  return 0;
   int nregs = THREAD_STATE_MAX;
    if (!check_kern_call(thread_get_state(t, flavor,  (unsigned int *)regs, (unsigned int*)&nregs))) {
      sprintf(error_string, "%s, while trying to get thread state", mach_error_string);
      return 0;
    }
   if (verbose)
     printf_and_flush("thread_get_state: flavor = %d, regs = 0x%x, nregs = %d\n", flavor, (unsigned int)regs, nregs);
   return nregs;
  }
  
            
  bool set_thread_state(int pid, int flavor, thread_state_data_t regs, int len) {
    thread_t t = get_thread_for_pid(pid);
    if (t == 0) return false;
    if (verbose)
       printf_and_flush("thread_set_state: flavor = %d, regs = 0x%x, len = %d\n", flavor, (unsigned int)regs, len);
    if (!check_kern_call(thread_set_state(t, flavor, (unsigned int*)regs, len))) {
      sprintf(error_string, "%s, while trying to set thread state", mach_error_string);
      return false;
    }
    return true;
  }
  
  
  void wait_till_thread_is_waiting(int pid) {
    struct thread_basic_info info;
    while (thread_info(pid, THREAD_BASIC_INFO, (thread_info_t)&info)
    &&    info.run_state != TH_STATE_WAITING)
      {}
  }


  vm_address_t allocate_memory(int pid, vm_address_t address, vm_size_t size, boolean_t anywhere) {
    task_t target_task = get_task_for_pid(pid);
    if (target_task == 0) return (vm_address_t)-1;

    if (!check_kern_call(vm_allocate(target_task, &address, size, anywhere))) {
      sprintf(error_string, "%s, while trying to allocate debuggee memory",
              mach_error_string);
      return (vm_address_t)-1;
    }
    return address;
  }
  

  bool thread_info(int pid, int flavor, thread_info_t the_thread_info) {
    thread_t t = get_thread_for_pid(pid);
    if (t == 0)  return false;
    unsigned int thread_info_count;
    switch (flavor) {
     case THREAD_BASIC_INFO:  thread_info_count = THREAD_BASIC_INFO_COUNT;  break;
     default: error_printf_and_flush( "thread_info: " "bad flavor for thread info %d", flavor); exit(1);
    }
    if (!check_kern_call(::thread_info(t, flavor,  the_thread_info, &thread_info_count))) {
      sprintf(error_string, "%s, while trying to get thread ifno", mach_error_string);
      return false;
    }
    return true;
  }

   
  // Works hard to meet Mach's requirement that kernel call be page-aligned and may not return the whole thing.
  // dmu 11/1
  // caller must free buffer later with free_memory_buffer
  void get_memory(int pid, AddressInterval requested, 
                   char*&        buffer_to_free,
                   unsigned int& length_to_free,
                   char*&        start_of_result,
                   unsigned int& length_of_result) {
	
    task_t t = get_task_for_pid(pid);
		
     
	if (t == 0) return;
    
	
    // Mach only does this on page boundaries
    AddressInterval rounded = requested.round(vm_page_size);
        
    // read enlarged section
    buffer_to_free  = NULL; length_to_free  = 0;
    start_of_result = NULL; length_of_result = 0;
    if (verbose) 
		printf_and_flush("vm_read(%d 0x%x 0x%x 0x%x 0x%x)\n",
						t, rounded.bottom(), rounded.length(),
						(int)&buffer_to_free, (int)&length_to_free);
    
	if (!check_kern_call(
         vm_read(t, 
                 rounded.bottom(), rounded.length(), 
                (vm_offset_t*)& buffer_to_free, &length_to_free))) {
      sprintf(error_string, "%s, while trying to read debuggee's memory: "
                            "vm_read(%d 0x%x 0x%x 0x%x 0x%x)",
                             mach_error_string,
                             t, rounded.bottom(), rounded.length(),
                             (int)&buffer_to_free, (int)&length_to_free);
      return;
    }
    unsigned int extra_bottom = requested.bottom() - rounded.bottom();
    start_of_result = buffer_to_free + extra_bottom; 
    unsigned int top_address_read = rounded.bottom() + length_to_free;
    length_of_result = min(requested.length(), top_address_read - requested.bottom());   
  }


  bool free_memory_buffer(char* addr, unsigned int len) {
    if (addr == NULL)
      return true;
    if (check_kern_call( vm_deallocate(mach_task_self(),  (vm_address_t)addr, len)))
      return true;
    sprintf(error_string, "%s, while trying to free memory contents", mach_error_string);
    return false;
  }
 
  void set_memory(int pid, char* data_from_caller, AddressInterval requested) {
    task_t t = get_task_for_pid(pid);
    if (t == 0) return;
    
    AddressInterval enlarged = requested.round(vm_page_size);
    // read first and last pages
    char* buf_for_vm_write = NULL;
                 
    if (verbose) printf_and_flush("set_memory: req bot = 0x%x, req size = 0x%x, page size = 0x%x\n"
                        "enlarged bot = 0x%x, enlarged size = 0x%x\n", 
                        requested.bottom(), requested.length(), vm_page_size,
                        enlarged.bottom(), enlarged.length());
    if (requested == enlarged) {
      buf_for_vm_write = data_from_caller;
      if (verbose) printf_and_flush("requested == enlarged\n");
    }
    else if (!check_kern_call( vm_allocate(mach_task_self(), (vm_offset_t*)&buf_for_vm_write, enlarged.length(), true))) {
      sprintf(error_string, "%s, while trying to allocate set_memory buffer", mach_error_string);
      return;
    }
    else if (! fill_in_buf_to_write(t, buf_for_vm_write, data_from_caller, requested, enlarged)) {
	    if (verbose) printf_and_flush(" fill_in_buf_to_write returned false ");
        return;
	}
    check_kern_call(vm_write(t, enlarged.bottom(), (vm_offset_t)buf_for_vm_write, enlarged.length()));
    if (buf_for_vm_write != data_from_caller) 
      free_memory_buffer(buf_for_vm_write, enlarged.length());
  }

  


 private:
  bool fill_in_buf_to_write(task_t t, char* buf_to_fill, char* new_data,
                            AddressInterval requested, AddressInterval enlarged) { 
    if (verbose) printf_and_flush("requested != enlarged\n");
    char* first_page_buf = NULL;
    unsigned int extra_bottom = requested.bottom() - enlarged.bottom();
    unsigned int extra_top    = enlarged.top() - requested.top();
    if (enlarged.bottom() != requested.bottom()) {
      first_page_buf = read_a_page(t, enlarged.bottom());
      if (first_page_buf == NULL)
        return false;
      bcopy(first_page_buf, buf_to_fill, extra_bottom);
    }
    if (enlarged.top() != requested.top()) {
      char* last_page_buf =
        enlarged.bottom() + vm_page_size == enlarged.top()  &&  first_page_buf != NULL
          ? first_page_buf
          : read_a_page(t, enlarged.top() - vm_page_size);
      if (last_page_buf == NULL)
        return false;
      bcopy(last_page_buf + vm_page_size - extra_top,
            buf_to_fill + enlarged.length() - extra_top,
            extra_top);
      if (first_page_buf != last_page_buf)
        free_memory_buffer(last_page_buf, vm_page_size);
    }
    free_memory_buffer(first_page_buf, vm_page_size);
    bcopy(new_data, buf_to_fill + extra_bottom, requested.length());
    return true;
  }
    
      
 private:
  char* read_a_page(thread_t t, unsigned int addr) {
    if (verbose) printf_and_flush("read_a_page: addr: %d\n", addr);
    
    char* buf = NULL;
    unsigned int len = 0;
    if (!check_kern_call(  vm_read(t, addr, vm_page_size, (vm_offset_t*)&buf, &len))) {   
      sprintf(error_string, "%s, while trying to read page at %d (0x%x)", mach_error_string, addr, addr);
      return NULL;
    }
    if (len != vm_page_size) {
      free_memory_buffer(buf, len);
      sprintf(error_string, "could not read whole page (%d vs. %d)", len, vm_page_size);
      return NULL;
    }
    return buf;
  }


 protected:
  bool single_step(int pid, int addr) {
    do {
      errno = 0;
      my_ptrace(PT_STEP, pid, (char*)addr, 0);
    } while (errno == EBUSY); // for multistepping, process may already be running
    int e = errno;
    if (e != 0) {
      char* status = build_errno_status(e, "ptrace single step");
      sprintf(error_string, "%s", status);
      delete status;
      return false;
    }
    wait_till_thread_is_waiting(pid);
    return error_string[0] == '\0';
  }


  bool multi_step(int pid, int addr, int maxSteps, int targetAddr) {
    int last_pc = -1;
	if (verbose) printf_and_flush("looking for addr during multi-step: 0x%x\n",targetAddr);
	if (verbose) printf_and_flush("addr: 0x%x\n",addr);
	if (verbose) printf_and_flush("PC before multi-step: 0x%x\n",get_pc(pid));
    for (int nSteps = 0;  nSteps < maxSteps;  ++nSteps) {
      int pc = get_pc(pid);
	  if (verbose) printf_and_flush("pc is now: 0x%x\n",pc);
      if (pc == 0  ||  pc == targetAddr || pc == last_pc)
        break;
      if (!single_step(pid, addr))
        break;
      addr = 1; // continue from where you are thereafter
      last_pc = pc;
    }
    return error_string[0] == '\0';
  }
 protected:
  char error_string[10000];
 private:
  char mach_error_string[10000];
    
  bool check_kern_call(kern_return_t ret) {
    if (ret == KERN_SUCCESS) { 
      mach_error_string[0] = '\0'; 
      return true; 
    }
    if (::mach_error_string(ret) != NULL)
      strcpy (mach_error_string,::mach_error_string(ret));
    else
      sprintf(mach_error_string, "Mach error %d",    ret);
    return false;
  } 
};


class MachInterface: public SocketUser, public MemoryManipulator {
 public:
    MachInterface(BufferedSocket& ss) : SocketUser(ss) {
	 error_string[0] = '\0';
    }
 protected:
   void write_error_string() { s. write_string(error_string, "writing error"); }
   void loadMemoryFromSocket (int pid, Socket* inSocket) {
	// read in memory ranges array
	int rangesArrayLength;
	int* rangesArray = (int*) inSocket->read_bytes (rangesArrayLength, "reading memory range array from file");

	//  ***** Setting the memory contents *****
	
    // Loop over memory ranges, setting memory contents as we go along
	int memoryBufferLength = 0;
	int rangesArrayIntegerLength = rangesArrayLength/sizeof(int);
	for(int i = 0; i < rangesArrayIntegerLength/2; ++i) {
		printf_and_flush("loading i: %d, base: %d length: %d \n", i, rangesArray[2*i], memoryBufferLength);
		char* memoryBuffer = inSocket->read_bytes(memoryBufferLength, "reading memory region from file to buffer");
		set_memory(pid, memoryBuffer, AddressInterval(rangesArray[2*i], memoryBufferLength));
        delete memoryBuffer;
	}
   }

};


class TaskForPID: public MachInterface {
 public:
   TaskForPID(BufferedSocket& ss) : MachInterface(ss) {}
   
   void do_it() {
     int pid = s.read_int(         "getting pid");
     if (verbose) printf_and_flush("geting task for pid %d\n", pid);
     task_t r = get_task_for_pid(pid);
     if (verbose) printf_and_flush("writing task %d\n", r);
     s.write_int(int(r),          "writing task");
     if (verbose) printf_and_flush("wrote task\n");
     if (verbose) printf_and_flush("writing error string <%s>\n", error_string);
     write_error_string();
   }
 };
 
 
class ThreadsForTask: public MachInterface {
 public:
  ThreadsForTask(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    task_t target = (task_t)s.read_int("getting task");
    thread_array_t threads = NULL;  unsigned int nthreads = 0;
    get_task_threads(target, threads, nthreads);
    s.write_int_array((int*)threads, (int)nthreads, "writing threads");
    if (!free_thread_array(threads, nthreads)) {
      error_printf_and_flush( "ThreadsForTask: " "%s", error_string);
      exit(1);
    }
    write_error_string();
  }
};


class IntegerStateGetter: public MachInterface {
 public:
  IntegerStateGetter(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid    = s.read_int("getting pid");
    thread_state_data_t regs;
    int nregs = get_thread_state(pid, MACHINE_THREAD_STATE, regs);
    write_error_string();
    if (error_string[0] == '\0')
      s.write_int_array((int *)regs, nregs, "writing registers");
  }
};


class IntegerStateSetter: public MachInterface {
 public:
  IntegerStateSetter(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid    = s.read_int("getting pid");
    int* regs;
    int len;
    s.read_int_array(regs, len, "getting integer state");
    set_thread_state(pid, MACHINE_THREAD_STATE, (natural_t *)regs, len);
    write_error_string();
  }
};


class MemoryGetter: public MachInterface {
 public:
  MemoryGetter(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid    = s.read_int("getting pid");
    unsigned int addr   = s.read_int("getting addr");
    unsigned int len    = s.read_int("getting length");
    char* buf_to_free   = NULL;   unsigned int length_to_free = 0;
    char* data_for_user = NULL;   unsigned int length_for_user = 0;
    get_memory(pid, AddressInterval(addr, len), buf_to_free, length_to_free, data_for_user, length_for_user);
    s.write_bytes(data_for_user, length_for_user, "writing memory");
    free_memory_buffer(buf_to_free, length_to_free);
    write_error_string();
  }
};


class MemorySetter: public MachInterface {
 public:
  MemorySetter(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid    = s.read_int("getting pid");
    int len = 0;
    char* buf = s.read_bytes(len, "reading memory contents");
    int addr = s.read_int("getting address");
    set_memory(pid, buf, AddressInterval(addr, len));
    write_error_string();
    delete buf;
  }
};


class CompactSnapshotTaker: public MachInterface {
 public:
  CompactSnapshotTaker(BufferedSocket& ss) : MachInterface(ss) {}
  //File contents are:
  // 1). Register data
  // 2). Array of memory ranges saved
  // 3). Contents of the memory ranges
  
  void do_it() {
	int pid = s.read_int("getting pid");
	int len;
	int* memoryRanges  = (int*) s.read_bytes(len, "reading memory ranges"); //this line sets the len value, too
	char* file_name    = s.read_string("getting file name");
	FileHandler* outFile = new FileHandler (file_name, O_CREAT | O_RDWR, S_IROTH|S_IWOTH|S_IRUSR|S_IWUSR);
		
	// *** Getting the integer-state,   writing it to file     ***
	thread_state_data_t regs;
	int bytesPerRegister = 4;
	unsigned int nregs   = get_thread_state(pid, MACHINE_THREAD_STATE, regs); // is slower than nregs = 40, 
                                                                                  // but in this case, we need regs to be set. Ausch, Nov/04
	outFile->write_data ( (char *) regs, (nregs * bytesPerRegister), "writing out register array");
	
	outFile->write_bytes ((char*) memoryRanges, len, "writing out memory ranges array");
	
	// *** Getting the memory contents, appending them to file ***
	unsigned int addr        = 0;
	unsigned int lenFromAddr = 0;
	int lengthInIntegers = len/(sizeof(int));
	for (int i = 0; i  <  (lengthInIntegers/2); ++i) {
		addr = memoryRanges [2*i];
		lenFromAddr = memoryRanges[2*i+1] - addr;
		printf_and_flush( "Length: %d\n", lenFromAddr);
		char* buf_to_free   = NULL;   unsigned int length_to_free = 0;
		char* data_for_user = NULL;   unsigned int length_for_user = 0;
		get_memory(pid, AddressInterval(addr, lenFromAddr), buf_to_free, length_to_free, data_for_user, length_for_user);
		outFile->write_bytes(data_for_user, length_for_user, "writing out memory contents");
		free_memory_buffer(buf_to_free, length_to_free);
	}
	
	outFile->close_file();
	delete file_name;
    write_error_string();
  }
};

 
extern void breakpoint();

class CompactSnapshotLoader: public MachInterface {
 public:
 
  CompactSnapshotLoader(BufferedSocket& ss) : MachInterface(ss) {}
  
  void do_it() {
	int                 pid = s.read_int("getting pid");
	char*         file_name = s.read_string("getting file name");
	FileHandler*     inFile = new FileHandler (file_name, O_RDONLY);
	
	
	int				  nregs = 40; // Optimization. 
				    // nregs = get_thread_state(pid, MACHINE_THREAD_STATE, regs); is much slower - it returns
					// register content inside variable 'regs' - Ausch
	int    bytesPerInteger  = 4;
	int   registerSpaceSize = nregs * bytesPerInteger;	

	
	// ***** Getting/Setting the register contents *****
	thread_state_data_t regBuf; 
	inFile->read_data((char*)regBuf, registerSpaceSize, registerSpaceSize, "reading register array from file");
	if (!set_thread_state(pid, MACHINE_THREAD_STATE, regBuf, nregs)) printf_and_flush ("thread state setting failed");
	
	// read in memory ranges array
	int rangesArrayLength;
	int* rangesArray = (int*) inFile->read_bytes (rangesArrayLength, "reading memory range array from file");

	//  ***** Setting the memory contents *****
	
    // Loop over memory ranges, setting memory contents as we go along
	int memoryBufferLength = 0;
	int rangesArrayIntegerLength = rangesArrayLength/sizeof(int);
	for(int i = 0; i < rangesArrayIntegerLength/2; ++i) {
		printf_and_flush("loading i: %d, base: %d length: %d \n", i, rangesArray[2*i], memoryBufferLength);
		char* memoryBuffer = inFile->read_bytes(memoryBufferLength, "reading memory region from file to buffer");
		set_memory(pid, memoryBuffer, AddressInterval(rangesArray[2*i], memoryBufferLength));
        delete memoryBuffer;
	}
	
	inFile -> close_file();
	delete file_name;
	write_error_string();
  }
};


class FileRemover: public MachInterface {
 public:
  FileRemover(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    char* file_name = s.read_string("getting file name");
	remove(file_name);
	write_error_string();
  }
};


class RunStateGetter: public MachInterface {
 public:
  RunStateGetter(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid    = s.read_int("getting pid");
    struct thread_basic_info info;
    if (!thread_info(pid, THREAD_BASIC_INFO, (thread_info_t)&info)) {
      info.run_state = -1;
    }
    s.write_int(info.run_state, "writing run_state");
    write_error_string();
  }
};


class SingleStepper: public MachInterface {
 public:
  SingleStepper(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid    = s.read_int("getting pid");
    int addr   = s.read_int("getting continuation addr");
    single_step(pid, addr);
    write_error_string();
  }
};


class MultiStepper: public MachInterface {
 public:
  MultiStepper(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid        = s.read_int("getting pid");
    int addr       = s.read_int("getting continuation addr");
    int maxSteps   = s.read_int("getting max steps");
    int targetAddr = s.read_int("getting target addr");
    multi_step(pid, addr, maxSteps, targetAddr);
	write_error_string();
  }
};
    

class MemoryAllocator: public MachInterface {
 public:
  MemoryAllocator(BufferedSocket& ss) : MachInterface(ss) {}
  void do_it() {
    int pid = s.read_int("getting pid");
    int addr = s.read_int("getting address");
    int size = s.read_int("getting size");
    boolean_t anywhere = s.read_byte("getting anywhere flag");
    
	addr = allocate_memory(pid, addr, size, anywhere);
      
	s.write_int(addr, "writing address");
    write_error_string();
  }
};

class ImageDownloader: public MachInterface {
 public:
  ImageDownloader(BufferedSocket& ss) : MachInterface(ss) {}
  
  void do_it() {
	int pid   = s.read_int("getting pid");
//	int vmOop = s.read_int("getting vmOop");
	int len;
	int* memoryAddresses  = (int*) s.read_bytes(len, "reading memory addresses"); //this line sets the len value, too
		
	unsigned int addr        = 0;
	int lengthInIntegers = len/(sizeof(int));
	for (int i = 0; i  <  lengthInIntegers; ++i) {
		addr = memoryAddresses [i];
		int bufSize;
		char *buf = s.read_bytes(bufSize, "reading memory range");
		set_memory(pid, buf, AddressInterval(addr, bufSize));
		free_memory_buffer(buf, bufSize);
	}
//	set_VMOop(vmOop);
	write_error_string();
 }
};



class MachRequestServer: public SocketUser {
  public:
    MachRequestServer(BufferedSocket& ss) : SocketUser(ss) {}

    bool do_it() {
      mach_request_type_t request_type = (mach_request_type_t)s.read_byte("reading mach request type");
      if (verbose) printf_and_flush("servicing mach request type: %d\n", request_type);
      if (print_request_types  &&  string_for_mach_request_type(request_type))
        printf_and_flush("request type: %s (mach), pid: %d...\n",
                         string_for_mach_request_type(request_type), getpid());

      bool r = true;
      switch (request_type) {
       case mach_request_task_for_pid:          { TaskForPID           x(s);  x.do_it(); }  break;
       case mach_request_threads_for_task:      { ThreadsForTask       x(s);  x.do_it(); }  break;
       case mach_request_get_integer_state:     { IntegerStateGetter   x(s);  x.do_it(); }  break;
       case mach_request_set_integer_state:     { IntegerStateSetter   x(s);  x.do_it(); }  break;
       case mach_request_get_memory:            { MemoryGetter         x(s);  x.do_it(); }  break;
       case mach_request_set_memory:            { MemorySetter         x(s);  x.do_it(); }  break;
       case mach_request_run_state:             { RunStateGetter       x(s);  x.do_it(); }  break;
       case mach_request_single_step:           { SingleStepper        x(s);  x.do_it(); }  break;
	   case mach_request_download_image:		{ ImageDownloader		 x(s);  x.do_it(); }  r = true;  break;
	   //[todo cleanup] The things below need to be refactored -> they prolly aren't mach specific. Alex, Nov/04
       case mach_request_multi_step:            { MultiStepper         x(s);  x.do_it(); }  break;
       case mach_request_allocate_memory:       { MemoryAllocator      x(s);  x.do_it(); }  break;
	   case mach_request_remove_file:           { FileRemover          x(s);  x.do_it(); }  break;

	   // these rely on the mach integer state request, and are mach specific.
	   // could be refactored to be machine independent.
	   case mach_request_write_memory_to_file:          { CompactSnapshotTaker    x(s);  x.do_it(); } break;
	   case mach_request_read_memory_from_file:         { CompactSnapshotLoader   x(s);  x.do_it(); } break;
       
       default:  error_printf_and_flush( "MachRequestServer: " "bad mach request type: %d", request_type);  exit(1);  return false; /* for compiler */
      }
      if (print_request_types  &&  string_for_mach_request_type(request_type))
         printf_and_flush("request type: %s (mach), pid %d done\n",
           string_for_mach_request_type(request_type), getpid());
     return r;
    }
};

class LinuxRequestServer: public SocketUser {
  public:
    LinuxRequestServer(BufferedSocket& ss) : SocketUser(ss) {}

    bool do_it() {return false;}
};
