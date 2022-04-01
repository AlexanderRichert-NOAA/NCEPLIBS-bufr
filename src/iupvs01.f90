!> @file
!> @brief Read a data value from Section 0 or Section 1 of a BUFR message.

!> This function returns a specified value from within Section 0 or Section 1 of a BUFR message.
!>
!> <p>This function will work on any BUFR message encoded using BUFR edition 2, 3, or 4.
!> It is similar to function iupbs01(), except that iupbs01() operates on a BUFR message passed in via a memory array,
!> whereas this function operates on the BUFR message that was read into internal arrays via the most recent call to
!> any of the other [message-reading subroutines](@ref hierarchy) for a specified Fortran logical unit.
!>
!> @author J. Ator
!> @date 2005-11-29
!>
!> <b>Usage:</b> iupvs01( LUNIT, S01MNEM )
!>
!> @param[in]   LUNIT   -- integer: Fortran logical unit number for BUFR file
!> @param[in]  S01MNEM  -- character*(*): Value to be read from Section 0 or Section 1 of BUFR message in internal
!>                         arrays for LUNIT
!>                         - 'LENM'  = Length (in bytes) of BUFR message
!>                         - 'LEN0'  = Length (in bytes) of Section 0
!>                         - 'LEN1'  = Length (in bytes) of Section 1
!>                         - 'BEN'   = BUFR edition number
!>                         - 'BMT'   = BUFR master table
!>                         - 'OGCE'  = Originating center
!>                         - 'GSES'  = Originating subcenter
!>                         - 'USN'   = Update sequence number
!>                         - 'ISC2'  = Flag indicating absence/presence of optional) Section 2 in BUFR message:
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
!> @returns iupvs01 -- integer: Value corresponding to S01MNEM
!>                      - -1 = S01MNEM was invalid for the edition of BUFR message in internal arrays for LUNIT, or some
!>                             other error occurred
!>
!> @remarks
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
!> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator | Convert to Fortran90 |
!>

function iupvs01(LUNIT,S01MNEM)

      USE MODA_BITBUF

      use function_iupbs01

      CHARACTER*(*)   S01MNEM

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  Check the file status

      CALL STATUS(LUNIT,LUN,ILST,IMST)
      IF(ILST.EQ.0) GOTO 900
      IF(ILST.GT.0) GOTO 901
      IF(IMST.EQ.0) GOTO 902

!  Unpack the requested value

      IUPVS01 = IUPBS01(MBAY(:,LUN),S01MNEM)

!  EXITS
!  -----

      RETURN
900   CALL BORT('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: IUPVS01 - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

end
