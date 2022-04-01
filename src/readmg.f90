!> @file
!> @brief Read the next message from a BUFR file that was previously
!> opened for reading.

!> This subroutine reads the next BUFR message from logical unit
!> ABS(LUNXX) into internal arrays.
!>
!> <p>Logical unit ABS(LUNXX) should have already been opened for
!> input operations via a previous call to subroutine openbf().
!>
!> @authors J. Woollen
!> @authors J. Ator
!> @date 1994-01-06
!>
!> <b>Usage:</b> call readmg( LUNXX, SUBSET, JDATE, IRET )
!>
!> @param[in] LUNXX   -- integer: Absolute value is Fortran logical unit
!>                       number for BUFR file
!> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
!>                        message that was read
!>                        (see [DX BUFR Tables](@ref dfbftab)
!>                        for further information about Table A mnemonics)
!> @param[out] JDATE   -- integer: Date-time stored within Section 1 of
!>                        BUFR message that was read, in format of either
!>                        YYMMDDHH or YYYYMMDDHH, depending on the most
!>                        recent call to subroutine datelen()
!> @param[out] IRET    -- integer: return code
!>                           - 0 = new BUFR message was successfully
!>                                 read into internal arrays
!>                           - -1 = there are no more BUFR messages in
!>                                 the file connected to logical unit
!>                                 ABS(LUNXX)
!>
!> <p>Whenever this subroutine returns with IRET = 0, this indicates
!> that a new BUFR message of type SUBSET and date-time JDATE was
!> successfully read into internal arrays within the BUFRLIB
!> software, and from where it can then be easily manipulated or further
!> parsed via a call to one of the
!> [subset-reading subroutines](@ref hierarchy).  Otherwise,
!> if the subroutine returns with IRET = -1, then this indicates that
!> there are no more BUFR messages (i.e. end-of-file) within the file
!> connected to logical unit ABS(LUNXX).
!>
!> @remarks
!> - Any DX BUFR table messages encountered within ABS(LUNXX) will be
!> automatically processed and stored internally, so a successful return
!> from this subroutine will always result in a BUFR message containing
!> actual data values within the internal arrays.
!> - In prior versions of the BUFRLIB software, an input value of
!> LUNXX < 0 was an indicator to the subroutine to treat any read error
!> from ABS(LUNXX) the same as an end-of-file condition.  This option is
!> no longer supported, but the capability to call this subroutine with
!> LUNXX < 0 is itself still supported for backwards-compatibility with
!> certain legacy application programs. 
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1996-11-25 | J. Woollen | Modified to exit gracefully when the BUFR file is positioned after an "end-of-file" |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32; modified with semantic adjustments to ameliorate compiler complaints from Linux boxes |
!> | 2000-09-19 | J. Woollen | Removed logic that had been replicated in this and other read routines and consolidated it into a new routine cktaba(); maximum message length increased from 10,000 to 20,000 bytes |
!> | 2002-05-14 | J. Woollen | Removed entry point datelen() (it became a separate routine in the BUFRLIB) |
!> | 2003-11-04 | J. Ator    | Added documentation |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-11-29 | J. Ator    | Added rdmsgw() and rdmsgb calls to simulate readibm; added LUNXX < 0 option to simulate readft |
!> | 2009-03-23 | J. Ator    | Add logic to allow Section 3 decoding; add logic to process internal dictionary messages  |
!> | 2012-06-07 | J. Ator    | Don't respond to internal dictionary messages if Section 3 decoding is being used |
!> | 2012-09-15 | J. Woollen | Convert to C language I/O interface; remove code to reread message as bytes; replace Fortran BACKSPACE with C backbufr() |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_readmg

    private
    public readmg

    interface readmg
        module procedure readmg_4_d, readmg_8
    end interface

    contains

    subroutine readmg_4_d( lunxx, subset, jdate, iret )
!       used when call arguments to readmg are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunxx
        integer(kind=4), intent(out) :: jdate, iret
        character(len=*), intent(out) :: subset

        integer :: my_lunxx, my_jdate, my_iret

        my_lunxx = lunxx

        call readmg_body( my_lunxx, subset, my_jdate, my_iret )

        jdate = my_jdate
        iret = my_iret

    end subroutine readmg_4_d

    subroutine readmg_8( lunxx, subset, jdate, iret )
!       used when call arguments to readmg are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunxx
        integer(kind=8), intent(out) :: jdate, iret
        character(len=*), intent(out) :: subset

        integer :: my_lunxx, my_jdate, my_iret

        my_lunxx = lunxx

        call readmg_body( my_lunxx, subset, my_jdate, my_iret )

        jdate = my_jdate
        iret = my_iret

    end subroutine readmg_8

    subroutine readmg_body( lunxx, subset, jdate, iret )

        USE MODA_MSGCWD
        USE MODA_SC3BFR
        USE MODA_BITBUF

        use function_idxmsg

        COMMON /QUIET / IPRT

        CHARACTER*128 ERRSTR
        CHARACTER*8 SUBSET

        IRET = 0
        LUNIT = ABS(LUNXX)

!       Check the file status

        CALL STATUS(LUNIT,LUN,IL,IM)
        IF(IL.EQ.0) GOTO 900
        IF(IL.GT.0) GOTO 901
        CALL WTSTAT(LUNIT,LUN,IL,1)

!       Read a message into the internal message buffer

1       CALL RDMSGW(LUNIT,MBAY(:,LUN),IER)
        IF(IER.EQ.-1) GOTO 200

!       Parse the message section contents

        IF(ISC3(LUN).NE.0) CALL READS3(LUN)
        CALL CKTABA(LUN,SUBSET,JDATE,IRET)

!       Look for a dictionary message

        IF(IDXMSG(MBAY(:,LUN)).NE.1) RETURN

!       This is an internal dictionary message that was generated by the BUFRLIB archive library software.

        IF(ISC3(LUN).NE.0) RETURN

!       Section 3 decoding isn't being used, so backspace the file pointer and then use subroutine RDBFDX to read in all such
!       dictionary messages (they should be stored consecutively!) and reset the internal tables.

        CALL BACKBUFR(LUN) 
        CALL RDBFDX(LUNIT,LUN)

        IF(IPRT.GE.1) THEN
        CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
        ERRSTR = 'BUFRLIB: READMG - INTERNAL DICTIONARY MESSAGE READ; ACCOUNT FOR IT THEN READ IN NEXT MESSAGE WITHOUT RETURNING'
        CALL ERRWRT(ERRSTR)
        CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
        CALL ERRWRT(' ')
        ENDIF

!       Now go read another message.

        GOTO 1

!       EOF on attempted read

200     CALL WTSTAT(LUNIT,LUN,IL,0)
        INODE(LUN) = 0
        IDATE(LUN) = 0
        SUBSET = ' '
        JDATE = 0
        IRET = -1
        RETURN

!    EXITS
!    -----

900     CALL BORT('BUFRLIB: READMG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901     CALL BORT('BUFRLIB: READMG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')

    end subroutine readmg_body

end module
