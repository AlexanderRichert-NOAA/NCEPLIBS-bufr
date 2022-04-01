!> @file
!> @brief Write a data value into Section 1 of a BUFR message.

!> This subroutines writes a specified value into a specified location
!> within Section 1 of a BUFR message, overwriting the value previously
!> stored in that location.
!>
!> <p>This subroutine will work on any BUFR message encoded using BUFR
!> edition 2, 3, or 4.  It is similar to subroutine pkvs01(), except
!> that it operates on a BUFR message passed in via a memory array,
!> whereas pkvs01() operates on BUFR messages stored internally within
!> the software.
!>
!> @authors J. Ator
!> @authors D. Keyser
!> @date 2005-11-29
!>	
!> @param[in]     IVAL   -- integer: Value to be stored
!> @param[in,out] MBAY   -- integer(*): BUFR message
!> @param[in]   S1MNEM   -- character*(*): Location in Section 1 of MBAY within which to store IVAL
!>                          - 'BMT'   = BUFR master table
!>                          - 'OGCE'  = Originating center
!>                          - 'GSES'  = Originating subcenter
!>                          - 'USN'   = Update sequence number
!>                          - 'MTYP'  = Data category
!>                          - 'MSBTI' = Data subcategory (international)
!>                          - 'MSBT'  = Data subcategory (local)
!>                          - 'MTV'   = Version number of master table
!>                          - 'MTVL'  = Version number of local tables
!>                          - 'YCEN'  = Year of century (1-100)
!>                          - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
!>                          - 'YEAR'  = Year (4-digit)
!>                          - 'MNTH'  = Month
!>                          - 'DAYS'  = Day
!>                          - 'HOUR'  = Hour
!>                          - 'MINU'  = Minute
!>                          - 'SECO'  = Second
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of MBAY.
!> - Values corresponding to S1MNEM = 'GSES' can only be stored within BUFR messages encoded using BUFR edition 3 or 4.
!> - Values corresponding to S1MNEM = 'YCEN' or 'CENT' can only be stored within BUFR messages encoded using BUFR edition 2 or 3.
!> - Values corresponding to S1MNEM = 'YEAR', 'SECO' or 'MSBTI' can only be stored within BUFR messages encoded using
!>   BUFR edition 4.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2005-11-29 | J. Ator    | Original author |
!> | 2006-04-14 | D. Keyser  | Added options for 'MTYP', 'MSBT', 'YEAR', 'MNTH', 'DAYS', 'HOUR', 'YCEN' and 'CENT' |
!> | 2022-02-01 | J. Ator    | Convert to Fortran90 |
!>

subroutine pkbs1(IVAL,MBAY,S1MNEM)

        use function_iupbs01

        integer :: MBAY(:)

	CHARACTER*(*)	S1MNEM

	CHARACTER*128	BORT_STR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!	Note that the following call to function IUPBS01 will ensure that subroutine WRDLEN has been called.

	IBEN = IUPBS01(MBAY,'BEN')

!	Determine where to store the value.

	CALL GETS1LOC(S1MNEM,IBEN,ISBYT,IWID,IRET)
	IF ( (IRET.EQ.0) .AND. &
      	     ( (S1MNEM.EQ.'USN') .OR. (S1MNEM.EQ.'BMT') .OR. (S1MNEM.EQ.'OGCE') .OR. (S1MNEM.EQ.'GSES') .OR. &
      	       (S1MNEM.EQ.'MTYP') .OR. (S1MNEM.EQ.'MSBTI') .OR. (S1MNEM.EQ.'MSBT') .OR. (S1MNEM.EQ.'MTV') .OR. &
      	       (S1MNEM.EQ.'MTVL') .OR. (S1MNEM.EQ.'YCEN') .OR. (S1MNEM.EQ.'CENT') .OR. (S1MNEM.EQ.'YEAR') .OR. &
      	       (S1MNEM.EQ.'MNTH') .OR. (S1MNEM.EQ.'DAYS') .OR. (S1MNEM.EQ.'HOUR') .OR. (S1MNEM.EQ.'MINU') .OR. &
      	       (S1MNEM.EQ.'SECO') ) ) THEN

!	    Store the value.

	    IBIT = (IUPBS01(MBAY,'LEN0')+ISBYT-1)*8
	    CALL PKB(IVAL,IWID,MBAY,IBIT)
	ELSE
	    GOTO 900
	ENDIF

	RETURN
900	WRITE(BORT_STR,'("BUFRLIB: PKBS1 - CANNOT OVERWRITE LOCATION CORRESPONDING TO MNEMONIC (",A,") WITHIN BUFR EDITION ' &
      	    // '(",I1,")")') S1MNEM, IBEN
      	CALL BORT(BORT_STR)

end
