!> @file
!> @brief Read a data value from Section 0 or Section 1 of a BUFR message.

!> This function returns a specified value from within Section 0 or Section 1 of a BUFR message.
!>
!> <p>This function will work on any BUFR message encoded using BUFR edition 2, 3, or 4.
!> It is similar to function iupvs01(), except that it operates on a BUFR message passed in via a memory array,
!> whereas iupvs01() operates on the BUFR message that was read into internal arrays via the most recent call to
!> any of the other [message-reading subroutines](@ref hierarchy) for a specified Fortran logical unit.
!>
!> @author J. Ator
!> @date 2005-11-29
!>
!> <b>Usage:</b> iupbs01( MBAY, S01MNEM )
!>
!> @param[in]  MBAY   -- integer(*): BUFR message
!> @param[in]  S01MNEM  -- character*(*): Value to be read from Section 0 or Section 1 of MBAY
!>                         - 'LENM'  = Length (in bytes) of BUFR message
!>                         - 'LEN0'  = Length (in bytes) of Section 0
!>                         - 'LEN1'  = Length (in bytes) of Section 1
!>                         - 'BEN'   = BUFR edition number
!>                         - 'BMT'   = BUFR master table
!>                         - 'OGCE'  = Originating center
!>                         - 'GSES'  = Originating subcenter
!>                         - 'USN'   = Update sequence number
!>                         - 'ISC2'  = Flag indicating absence/presence of (optional) Section 2 in BUFR message:
!>                                    - 0 = Section 2 absent
!>                                    - 1 = Section 2 present
!>                         - 'MTYP'  = Data category
!>                         - 'MSBTI' = Data subcategory (international)
!>                         - 'MSBT'  = Data subcategory (local)
!>                         - 'MTV'   = Version number of master table
!>                         - 'MTVL'  = Version number of local tables
!>                         - 'YCEN'  = Year of century (1-100)
!>                         - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
!>                         - 'YEAR'  = Year (4-digit)
!>                         - 'MNTH'  = Month
!>                         - 'DAYS'  = Day
!>                         - 'HOUR'  = Hour
!>                         - 'MINU'  = Minute
!>                         - 'SECO'  = Second
!> @returns iupbs01 -- integer: Value corresponding to S01MNEM
!>                      - -1 = S01MNEM was invalid for the edition of BUFR message in MBAY, or some other error occurred
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of MBAY.
!> - Values corresponding to S01MNEM = 'GSES' can only be read from BUFR messages encoded using BUFR edition 3 or 4.
!> - Values corresponding to S01MNEM = 'YCEN' or 'CENT' can only be read from BUFR messages encoded using BUFR edition 2 or 3.
!> - When reading from BUFR messages encoded using BUFR edition 2 or 3, values corresponding to S01MNEM = 'YEAR' will be
!>   calculated internally using the values for 'YCEN' and 'CENT', or inferred using a windowing technique.
!> - Values corresponding to S01MNEM = 'SECO' or 'MSBTI' can only be read from BUFR messages encoded using BUFR edition 4.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2005-11-29 | J. Ator | Original author |
!> | 2006-04-14 | J. Ator | Added options for 'YCEN' and 'CENT'; restructured logic |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module function_iupbs01

    private
    public iupbs01

    interface iupbs01
        module procedure iupbs01_4_d, iupbs01_8
    end interface

    contains

    function iupbs01_4_d( mbay, s01mnem )
!       used when call arguments to iupbs01 are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:)
        character(len=*), intent(in) :: s01mnem
        integer(kind=4) :: iupbs01_4_d

        integer :: my_mbay(size(mbay))

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        iupbs01_4_d = iupbs01_body( my_mbay, s01mnem )

    end function iupbs01_4_d

    function iupbs01_8( mbay, s01mnem )
!       used when call arguments to iupbs01 are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:)
        character(len=*), intent(in) :: s01mnem
        integer(kind=8) :: iupbs01_8

        integer :: my_mbay(size(mbay))

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        iupbs01_8 = iupbs01_body( my_mbay, s01mnem )

    end function iupbs01_8

    function iupbs01_body( MBAY, S01MNEM )

        use function_iupb

	integer, intent(in) :: MBAY(:)

	CHARACTER*(*)	S01MNEM

	LOGICAL		OK4CENT

!	The following statement function checks whether its input value contains a valid century value.

	OK4CENT(IVAL) = ((IVAL.GE.19).AND.(IVAL.LE.21))

!	Call subroutine WRDLEN to initialize some important information about the local machine, just in case
!	subroutine OPENBF hasn't been called yet.

	CALL WRDLEN

!	Handle some simple requests that do not depend on the BUFR edition number.

	IF(S01MNEM.EQ.'LENM') THEN
	    IUPBS01_BODY = IUPB(MBAY,5,24)
	    RETURN
	ENDIF

	LEN0 = 8
	IF(S01MNEM.EQ.'LEN0') THEN
	    IUPBS01_BODY = LEN0
	    RETURN
	ENDIF

!	Get the BUFR edition number.

	IBEN = IUPB(MBAY,8,8)
	IF(S01MNEM.EQ.'BEN') THEN
	    IUPBS01_BODY = IBEN
	    RETURN
	ENDIF

!	Use the BUFR edition number to handle any other requests.

	CALL GETS1LOC(S01MNEM,IBEN,ISBYT,IWID,IRET)
	IF(IRET.EQ.0) THEN
	    IUPBS01_BODY = IUPB(MBAY,LEN0+ISBYT,IWID)
	    IF(S01MNEM.EQ.'CENT') THEN

!		Test whether the returned value was a valid century value.

		IF(.NOT.OK4CENT(IUPBS01_BODY)) IUPBS01_BODY = -1
            ENDIF
        ELSE IF( (S01MNEM.EQ.'YEAR') .AND. (IBEN.LT.4) ) THEN

!	    Calculate the 4-digit year.

	    IYOC = IUPB(MBAY,21,8)
	    ICEN = IUPB(MBAY,26,8)

!	    Does ICEN contain a valid century value?

	    IF(OK4CENT(ICEN)) THEN

!               YES, so use it to calculate the 4-digit year. Note that, by international convention, the year 2000 was the 100th
!               year of the 20th century, and the year 2001 was the 1st year of the 21st century

		IUPBS01_BODY = (ICEN-1)*100 + IYOC
	    ELSE

!               NO, so use a windowing technique to determine the 4-digit year from the year of the century.

		IUPBS01_BODY = I4DY(MOD(IYOC,100)*1000000)/10**6
	    ENDIF
	ELSE
	    IUPBS01_BODY = -1
	ENDIF

	RETURN

    end function iupbs01_body

end module
