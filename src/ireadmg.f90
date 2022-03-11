!> @file
!> @brief Read the next message from a BUFR file that was previously
!> opened for reading.

!> This function calls BUFRLIB subroutine readmg() and passes
!> back its return code as the function value.
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> <b>Usage:</b> ireadmg( LUNIT, SUBSET, IDATE )
!>
!> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
!> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
!>                        message that was read
!>                       (see [DX BUFR Tables](@ref dfbftab)
!>                        for further information about Table A mnemonics)
!> @param[out] IDATE   -- integer: date-time stored within Section 1 of
!>                        BUFR message that was read, in format of either
!>                        YYMMDDHH or YYYYMMDDHH, depending on the most
!>                        recent call to subroutine datelen()
!> @returns ireadmg -- integer:
!>                        - 0 = new BUFR message was successfully
!>                              read into internal arrays
!>                        - -1 = there are no more BUFR messages in the
!>                               file connected to logical unit LUNIT
!>
!> @remarks
!> - The use of this function allows the return code from readmg() to be
!> used as the target variable within an iterative program loop.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1999-11-18 | J. Woollen | Added new function entry points ireadmm and ireadibm |
!> | 2002-05-14 | J. Woollen | Removed entry points icopysb, ireadft, ireadibm, ireadmm, ireadns and ireadsb (they became separate routines in the BUFRLIB) |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added history documentation |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module function_ireadmg

    private
    public ireadmg

    interface ireadmg
        module procedure ireadmg_4_d, ireadmg_8
    end interface

    contains

    function ireadmg_4_d( lunit, subset, idate )
!       used when call arguments to ireadmg are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunit
        integer(kind=4), intent(out) :: idate
        integer(kind=4) :: ireadmg_4_d
        character(len=*), intent(out) :: subset

        integer :: my_lunit, my_idate, ireadmg_body

        my_lunit = lunit

        ireadmg_4_d = ireadmg_body( my_lunit, subset, my_idate )

        idate = my_idate

    end function ireadmg_4_d

    function ireadmg_8( lunit, subset, idate )
!       used when call arguments to ireadmg are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunit
        integer(kind=8), intent(out) :: idate
        integer(kind=8) :: ireadmg_8
        character(len=*), intent(out) :: subset

        integer :: my_lunit, my_idate, ireadmg_body

        my_lunit = lunit

        ireadmg_8 = ireadmg_body( my_lunit, subset, my_idate )

        idate = my_idate

    end function ireadmg_8

end module

function ireadmg_body( lunit, subset, idate )

      use subroutine_readmg

      CHARACTER*8 SUBSET
      CALL READMG(LUNIT,SUBSET,IDATE,IRET)
      IREADMG_BODY = IRET
      RETURN

end
