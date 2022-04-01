!> @file
!> @brief Read a BUFR message from a memory array.

!> This subroutine is similar to subroutine readmg(), except that it
!> reads a BUFR message from an array already in memory, whereas
!> readmg() reads a BUFR message from a file on the local system.
!>
!> @authors J. Woollen
!> @authors J. Ator
!> @date 1995-06-28
!>
!> @param[in] MESG    -- integer(*): BUFR message
!> @param[in] LUNIT   -- integer: Fortran logical unit number for
!>                       BUFR file
!> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
!>                        message that was read
!>                        (see [DX BUFR Tables](@ref dfbftab)
!>                        for further information about Table A mnemonics)
!> @param[out] JDATE   -- integer: Date-time stored within Section 1 of
!>                        BUFR message that was read, in format of either
!>                        YYMMDDHH or YYYYMMDDHH, depending on the most
!>                        recent call to subroutine datelen()
!> @param[out] IRET    -- integer: return code
!>                           - 0 = MESG was successfully read
!>                           - 11 = MESG contained a DX BUFR table message
!>                           - -1 = MESG contained an unrecognized
!>                                  Table A message type
!>
!> <p>This subroutine looks and behaves a lot like subroutine readmg() 
!> except that here we have one additional input argument MESG which
!> contains the BUFR message to be read by the BUFRLIB software.
!> As such, this subroutine can be used in any context in which readmg()
!> might otherwise be used, and from that point on, the application
!> program can proceed with a call to one of the
!> [subset-reading subroutines](@ref hierarchy) (and then,
!> subsequently, to any of the
!> [values-reading subroutines](@ref hierarchy)), just
!> like if readmg() had been called instead.
!>
!> <p>When using this subroutine, it's still necessary for the
!> application program to have previously called subroutine openbf() in
!> order to associate a DX BUFR tables file with the messages that are
!> being input via MESG, and it's still also necessary to pass in the
!> relevant LUNIT value as a call argument, even though in this case
!> the subroutine will not actually try to read from the associated
!> Fortran logical unit.
!>
!> <p>If MESG contains a DX BUFR table message, the subroutine will
!> store the contents internally and use them to process any
!> future BUFR messages associated with LUNIT.  In this case, the
!> subroutine will return with IRET = 11, and any number of
!> DX BUFR table messages passed in via consecutive calls to this
!> subroutine will accumulate internally and be treated as a single DX
!> BUFR table, up until a call is made where MESG no longer contains a
!> DX BUFR table message.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1995-06-28 | J. Woollen | Original author |
!> | 1997-07-29 | J. Woollen | Modified to process GOES soundings from NESDIS |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant; improved machine portability |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32; increased the maximum number of possible descriptors in a subset from 1000 to 3000 |
!> | 2000-09-19 | J. Woollen | Removed logic that had been replicated in this and other read routines and consolidated it into a new routine cktaba(); maximum message length increased from 10,000 to 20,000 bytes |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2004-08-18 | J. Ator    | Modified 'BUFR' string test for portability to EBCDIC machines; maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-11-29 | J. Ator    | Use ichkstr() |
!> | 2009-03-23 | D. Keyser  | Call bort() in case of MBAY overflow |
!> | 2009-03-23 | J. Ator    | Add logic to allow Section 3 decoding; add logic to process dictionary messages |
!> | 2012-06-07 | J. Ator    | Don't respond to DX table messages if Section 3 decoding is being used |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_readerme

    private
    public readerme

    interface readerme
        module procedure readerme_4_d, readerme_8
    end interface

    contains

    subroutine readerme_4_d( mesg, lunit, subset, jdate, iret )
!       used when call arguments to readerme are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunit, mesg(:)
        integer(kind=4), intent(out) :: jdate, iret
        character(len=*), intent(out) :: subset

        integer :: my_lunit, my_jdate, my_iret, my_mesg(size(mesg))

        my_mesg(1:size(mesg)) = mesg(1:size(mesg))
        my_lunit = lunit

        call readerme_body( my_mesg, my_lunit, subset, my_jdate, my_iret )

        jdate = my_jdate
        iret = my_iret

    end subroutine readerme_4_d

    subroutine readerme_8( mesg, lunit, subset, jdate, iret )
!       used when call arguments to readerme are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunit, mesg(:)
        integer(kind=8), intent(out) :: jdate, iret
        character(len=*), intent(out) :: subset

        integer :: my_lunit, my_jdate, my_iret, my_mesg(size(mesg)) 

        my_mesg(1:size(mesg)) = mesg(1:size(mesg))
        my_lunit = lunit

        call readerme_body( my_mesg, my_lunit, subset, my_jdate, my_iret )

        jdate = my_jdate
        iret = my_iret

    end subroutine readerme_8

    subroutine readerme_body(MESG,LUNIT,SUBSET,JDATE,IRET)

        USE MODA_SC3BFR
        USE MODA_IDRDM
        USE MODA_BITBUF

        use function_idxmsg
        use function_iupbs3
        use function_nmwrd
        use subroutine_stbfdx

        COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
        COMMON /QUIET/  IPRT

        CHARACTER*128 BORT_STR,ERRSTR
        CHARACTER*8 SUBSET
        CHARACTER*1 CEC0(8)

        integer, intent(in) :: mesg(:)

        integer :: IEC0(2)

        LOGICAL ENDTBL

        EQUIVALENCE (IEC0,CEC0)

        IRET = 0

!       Check the file status.

        CALL STATUS(LUNIT,LUN,IL,IM)
        IF(IL.EQ.0) GOTO 900
        IF(IL.GT.0) GOTO 901
        CALL WTSTAT(LUNIT,LUN,IL, 1)

!       Copy the input message into the internal message buffer.

        IEC0(1) = MESG(1)
        IEC0(2) = MESG(2)
        LNMSG = NMWRD(IEC0)
        IF(LNMSG*NBYTW.GT.MXMSGL) GOTO 902
        DO I=1,LNMSG
          MBAY(I,LUN) = MESG(I)
        ENDDO

!       Confirm that the first 4 bytes of CEC0 contain 'BUFR' encoded in CCITT IA5 (i.e. ASCII).

        IF(ICHKSTR('BUFR',CEC0,4).NE.0) GOTO 903

!       Parse the message section contents.

        IF(ISC3(LUN).NE.0) CALL READS3(LUN)

        CALL CKTABA(LUN,SUBSET,JDATE,IRET)

        IF(ISC3(LUN).NE.0) RETURN

!       Check for a DX dictionary message.

!       A new DX dictionary table can be passed in as a consecutive set of DX dictionary messages.  Each message should be passed
!       in one at a time, via input argument MESG during consecutive calls to this subroutine, and will be processed as a single
!       dictionary table up until the next message is passed in which either contains no data subsets or else is a non-DX
!       dictionary message.

        ENDTBL = .FALSE.

        IF(IDXMSG(MBAY(:,LUN)).EQ.1) THEN

!	  This is a DX dictionary message that was generated by the BUFRLIB archive library software.

	  IF(IUPBS3(MBAY(:,LUN),'NSUB').EQ.0) THEN

!	    But it doesn't contain any actual dictionary information, so assume we've reached the end of the dictionary table.

	    IF(IDRDM(LUN).GT.0) THEN
	      ENDTBL = .TRUE.
            ENDIF
	  ELSE
	    IF(IDRDM(LUN).EQ.0) THEN

!	      This is the first DX dictionary message that is part of a new dictionary table.

	      CALL DXINIT(LUN,0)
	    ENDIF
	    IDRDM(LUN) = IDRDM(LUN) + 1
	    CALL STBFDX(LUN,MBAY(:,LUN))
	  ENDIF
        ELSE IF(IDRDM(LUN).GT.0) THEN

!	  This is the first non-DX dictionary message received following a string of DX dictionary messages, so assume we've
!         reached the end of the dictionary table.

	  ENDTBL = .TRUE.
        ENDIF

        IF(ENDTBL) THEN
	  IF ( IPRT .GE. 2 ) THEN
	  CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	  WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' ) &
            'BUFRLIB: READERME - STORED NEW DX TABLE CONSISTING OF (', IDRDM(LUN), ') MESSAGES;'
	  CALL ERRWRT(ERRSTR)
	  ERRSTR = 'WILL APPLY THIS TABLE TO ALL SUBSEQUENT DATA MESSAGES UNTIL NEXT DX TABLE IS PASSED IN'
	  CALL ERRWRT(ERRSTR)
	  CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
          CALL ERRWRT(' ')
	  ENDIF
	  IDRDM(LUN) = 0
	  CALL MAKESTAB
        ENDIF

!      EXITS
!      -----

        RETURN
900     CALL BORT('BUFRLIB: READERME - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901     CALL BORT('BUFRLIB: READERME - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
902     WRITE(BORT_STR,'("BUFRLIB: READERME - INPUT BUFR MESSAGE LENGTH", 1X,I6," BYTES) IS LARGER THAN LIMIT OF ",I6," BYTES")') &
         LNMSG*NBYTW,MXMSGL
        CALL BORT(BORT_STR)
903     CALL BORT('BUFRLIB: READERME - FIRST 4 BYTES READ FROM RECORD NOT "BUFR", DOES NOT CONTAIN BUFR DATA')

    end subroutine readerme_body

end module
