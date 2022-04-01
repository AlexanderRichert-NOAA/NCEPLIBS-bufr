!> @file
!> @brief Read the date-time from Section 1 of a BUFR message.

!> This function returns the date-time from within Section 1 of a BUFR message.
!>
!> <p>The function will work on any BUFR message encoded using BUFR edition 2, 3, or 4.
!>
!> @author J. Ator
!> @date 2005-11-29
!>
!> <b>Usage:</b> igetdate( MBAY, IYR, IMO, IDY, IHR )
!>
!> @param[in]  MBAY   -- integer(*): BUFR message
!> @param[out] IYR   -- integer: Year stored within Section 1 of MBAY, in format of either YY or YYYY, depending on
!>                      the most recent call to subroutine datelen()
!> @param[out] IMO   -- integer: Month stored within Section 1 of MBAY
!> @param[out] IDY   -- integer: Day stored within Section 1 of MBAY
!> @param[out] IHR   -- integer: Hour stored within Section 1 of MBAY
!> @returns igetdate   -- integer: Date-time stored within Section 1 of MBAY, in format of either YYMMDDHH or YYYYMMDDHH,
!>                        depending on the most recent call to subroutine datelen()
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of MBAY.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2005-11-29 | J. Ator  | Original author |
!> | 2022-02-01 | J. Ator  | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module function_igetdate

    private
    public igetdate

    interface igetdate
        module procedure igetdate_4_d, igetdate_8
    end interface

    contains

    function igetdate_4_d( mbay, iyr, imo, idy, ihr )
!       used when call arguments to igetdate are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:)
        integer(kind=4), intent(out) :: iyr, imo, idy, ihr
        integer(kind=4) :: igetdate_4_d

        integer :: my_mbay(size(mbay)), my_iyr, my_imo, my_idy, my_ihr

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        igetdate_4_d = igetdate_body( my_mbay, my_iyr, my_imo, my_idy, my_ihr )

        iyr = my_iyr
        imo = my_imo
        idy = my_idy
        ihr = my_ihr

    end function igetdate_4_d

    function igetdate_8( mbay, iyr, imo, idy, ihr )
!       used when call arguments to igetdate are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:)
        integer(kind=8), intent(out) :: iyr, imo, idy, ihr
        integer(kind=8) :: igetdate_8

        integer :: my_mbay(size(mbay)), my_iyr, my_imo, my_idy, my_ihr

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        igetdate_8 = igetdate_body( my_mbay, my_iyr, my_imo, my_idy, my_ihr )

        iyr = my_iyr
        imo = my_imo
        idy = my_idy
        ihr = my_ihr

    end function igetdate_8

    function igetdate_body(MBAY,IYR,IMO,IDY,IHR)

        use function_iupbs01

	COMMON /DATELN/ LENDAT

        integer, intent(in) :: MBAY(:)

	IYR = IUPBS01(MBAY,'YEAR')
	IMO = IUPBS01(MBAY,'MNTH')
	IDY = IUPBS01(MBAY,'DAYS')
	IHR = IUPBS01(MBAY,'HOUR')
	IF(LENDAT.NE.10) THEN
	    IYR = MOD(IYR,100)
	ENDIF
	IGETDATE_BODY = (IYR*1000000) + (IMO*10000) + (IDY*100) + IHR

	RETURN

    end function igetdate_body

end module
