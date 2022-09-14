!> @file
!> @brief Define signatures to enable a number of C BUFRLIB functions to be accessed
!>        via wrapper functions from Fortran application programs.
!> 
!> @author J. Ator
!> @date 2022-09-01

module bufr_interface

  implicit none

  interface

!>  @brief Wraps BUFRLIB "cobfl" subroutine.
!>
!>  @author J. Ator
!>  @date 2022-09-01
!>
!>  @param[in] bfl -- character(*): [path]/name of system file to be opened
!>  @param[in] io -- character: Flag indicating how bfl is to be opened
!>
    subroutine cobfl_c( bfl, io ) bind(C, name='cobfl')
      use iso_c_binding
      character(len=1), intent(in) :: bfl(*)
      character(len=1), intent(in), value :: io
    end subroutine cobfl_c

!>  @brief Wraps BUFRLIB "crbmg" subroutine.
!>
!>  @author J. Ator
!>  @date 2022-09-01
!>
!>  @param[in] mxmb -- c_int: Dimensioned size of bmg array
!>  @param[out] bmg -- character(*): BUFR message
!>  @param[out] nmb -- c_int: Size of BUFR message in bmg array
!>  @param[out] iret -- c_int: Return code
!>
    subroutine crbmg_c( bmg, mxmb, nmb, iret ) bind(C, name='crbmg')
      use iso_c_binding
      character(len=1), intent(out) :: bmg(*)
      integer(c_int), intent(in), value :: mxmb
      integer(c_int), intent(out) :: nmb, iret
    end subroutine crbmg_c

  end interface

end module
