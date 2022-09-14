/** @file
 *  @brief Compute a unique 1-dimensional array index from 2-dimensional indices.
 */
#include "bufrlib.h"

/**
 *  This function computes a unique 1-dimensional array index from 2-dimensional indices, which in turn allows
 *  a 2-dimensional (row-by-column) array to be stored and accessed as a 1-dimensional array.
 *
 *  @author J. Ator
 *  @date 2009-03-23
 *
 *  @param[in] ii  --  int: First (row) index
 *  @param[in] jj  --  int: Second (column) index
 *  @param[in] numjj  --  int: Maximum number of column indices
 *
 *  @returns icvidx -- int: 1-dimensional index
 *
 *  <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2009-03-23 | J. Ator | Original author |
 */
int icvidx( int ii, int jj, int numjj )
{
	return ( numjj * ii ) + jj;
}
