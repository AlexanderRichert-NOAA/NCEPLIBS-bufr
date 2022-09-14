/** @file
 *  @brief Define signatures and declare variables for reading or writing BUFR
 *  messages via a C language interface.
 *
 *  These signatures and variables are used by the C language interface which
 *  encompasses subroutines cobfl(), crbmg(), cwbmg(), ccbfl() and rbytes().
 *
 *  @author J. Ator
 *  @date 2005-11-29
 */
#ifdef UNDERSCORE
#define cwbmg      cwbmg_
#define ccbfl      ccbfl_
#endif

void cobfl( char *, char );
void ccbfl( void );
void crbmg( char *, int, int *, int * );
void cwbmg( char *, f77int *, f77int * );
int rbytes( char *, int, int, int );

/** @var pbf
 *  File pointers
 */
#ifdef IN_COBFL
    FILE *pbf[2];  /* each element will automatically initialize to NULL */
#else
    extern FILE *pbf[2];
#endif
