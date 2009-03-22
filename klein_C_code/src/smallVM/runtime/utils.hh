# ifndef UTILS_HH
# define UTILS_HH

inline unsigned int max(unsigned int a, unsigned int b) { return a > b ? a : b; }
inline unsigned int min(unsigned int a, unsigned int b) { return a < b ? a : b; }

inline int divide_and_round_up(int n, int modulus) { return ( n + ( modulus - 1) )  /  modulus; }

int length_of_C_string( char* s );
int arg_count_of_string( char* s, int len );

# endif // UTILS_HH