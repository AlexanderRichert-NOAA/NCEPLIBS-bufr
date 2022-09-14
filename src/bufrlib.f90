!> @file
!> @brief Define signatures and declare macros for use throughout the Fortran portion of the BUFRLIB.
!>
!> @author J. Ator
!> @date 2022-09-01

module bufrlib

  use bufr_interface

  implicit none

  interface

!>  @brief Wraps BUFRLIB "icvidx" function.
!>
!>  @author J. Ator
!>  @date 2022-09-01
!>
!>  @param[in] ii -- c_int: First (row) index
!>  @param[in] jj -- c_int: Second (column) index
!>  @param[in] numjj -- c_int: Maximum number of column indicess
!>  @returns icvidx_c -- c_int: 1-dimensional index
!>
    integer(c_int) function icvidx_c( ii, jj, numjj ) bind(C, name='icvidx')
      use iso_c_binding
      integer(c_int), intent(in), value :: ii, jj, numjj
    end function icvidx_c

  end interface

end module
