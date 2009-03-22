# ifndef SMALL_SELF_TYPES_HH
# define SMALL_SELF_TYPES_YY

// Whatever is fastest on this machine
typedef          int fint;
typedef unsigned int u_fint;

typedef          int int32;
typedef unsigned int u_int32;

typedef          short int16;
typedef unsigned short u_int16;

#   if OOP_SIZE == 16
                       typedef   int16   smi;
                       typedef u_int16 u_smi;
# elif OOP_SIZE == 32
                       typedef   int32   smi; 
                       typedef u_int32 u_smi;
# else
                       # error Oop_Size?
# endif


typedef smi oop_t;


# ifndef NULL
  # define NULL 0
# endif

# endif // SMALL_SELF_TYPES_HH