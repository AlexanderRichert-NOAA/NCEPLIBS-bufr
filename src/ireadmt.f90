!> @file
!> @brief Check whether master BUFR tables need to be read from the local file system

!> This function checks the most recent BUFR message that was read via a call to one of the
!> [message-reading subroutines](@ref hierarchy) and determines whether the appropriate corresponding BUFR master tables
!> have already been read into internal memory.
!>
!> <p>If not, then it opens the appropriate master BUFR tables on the local file system and then reads the contents into
!> internal memory, clearing any previous master BUFR table information that may have previously been stored there.
!>
!> @author J. Ator
!> @date 2009-03-23
!>
!> @param[in]  LUN     -- integer: Internal I/O stream index associated with BUFR file
!> @returns ireadmt    -- integer: Flag indicating whether new master BUFR tables needed to be read into internal memory:
!>                        - 0 = No
!>                        - 1 = Yes
!>
!> <p>Information about the location of master BUFR tables on the local file system is obtained from the most recent call to
!> subroutine mtinfo(), or else from subroutine bfrini() if subroutine mtinfo() was never called, and in which case Fortran
!> logical unit numbers 98 and 99 will be used by this function for opening and reading master BUFR table files.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2009-03-23 | J. Ator  | Original author |
!> | 2014-11-25 | J. Ator  | Add call to cpmstabs() for access to master table information within C when using dynamically-allocated arrays |
!> | 2017-10-13 | J. Ator  | Add functionality to check whether new master tables need to be read (this functionality was previously part of subroutine reads3()) |
!> | 2018-04-09 | J. Ator  | Only read master B and D tables when Section 3 is being used for decoding |
!> | 2022-02-01 | J. Ator  | Convert to Fortran90 |
!>

integer function ireadmt ( LUN )

        USE MODV_MAXNC

	USE MODA_MSTABS
	USE MODA_BITBUF
	USE MODA_RDMTB
	USE MODA_SC3BFR

        use function_iupbs01
        use subroutine_upds3

	COMMON /QUIET/  IPRT
	COMMON /MSTINF/ LUN1, LUN2, LMTD, MTDIR
	COMMON /TABLEF/ CDMF

	CHARACTER*1	CDMF
	CHARACTER*6	CDS3(MAXNC)
	CHARACTER*100	MTDIR
	CHARACTER*128	BORT_STR
	CHARACTER*132	STDFIL,LOCFIL
	LOGICAL		ALLSTD

!       Initializing the following value ensures that new master tables are read during the first call to this subroutine.

	DATA    LMT /-99/

	SAVE    LMT, LMTV, LOGCE, LMTVL

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

	IREADMT = 0

! 	Unpack some Section 1 information from the message that was most recently read.

	IMT  = IUPBS01 ( MBAY(:,LUN), 'BMT' )
	IMTV = IUPBS01 ( MBAY(:,LUN), 'MTV' )
	IOGCE = IUPBS01 ( MBAY(:,LUN), 'OGCE' )
	IMTVL = IUPBS01 ( MBAY(:,LUN), 'MTVL' )

! 	Compare the master table and master table version numbers from 	this message to those from the message that was processed
! 	during the previous call to this subroutine.

	IF (  ( IMT .NE. LMT ) &
      		.OR. &
      	    ( ( IMT .NE. 0 ) .AND. ( IMTV .NE. LMTV ) ) &
      		.OR. &
      	    ( ( IMT .EQ. 0 ) .AND. ( IMTV .NE. LMTV ) .AND. ( ( IMTV .GT. 13 ) .OR. ( LMTV .GT. 13 ) ) )  ) &
      	  THEN

! 	  Either the master table number has changed
! 	        .OR.
! 	  The master table number hasn't changed, but it isn't 0, and the table version number has changed
! 	        .OR.
! 	  The master table number hasn't changed and is 0, but the table version number has changed, and at least one of the table
! 	  version numbers (i.e. the current or the previous) is greater than 13 (which is the last version that was a superset of
! 	  all earlier versions of master table 0!)

! 	  In any of these cases, we need to read in new tables!

	  IREADMT = 1
	ELSE

! 	  Unpack the list of Section 3 descriptors from the message and determine if any of them are local descriptors.

	  CALL UPDS3 ( MBAY(:,LUN), MAXNC, CDS3, NCDS3 )
	  II = 1
	  ALLSTD = .TRUE.
	  DO WHILE ( (ALLSTD) .AND. (II.LE.NCDS3) )
	    IF ( ISTDESC(IFXY(CDS3(II))) .EQ. 0 ) THEN
	      ALLSTD = .FALSE.
	    ELSE
	      II = II + 1
	    ENDIF
	  ENDDO

! 	  If there was at least one local (i.e. non-standard) descriptor, and if either the originating center or local table
! 	  version number are different than those from the message that was processed during the previous call to this subroutine,
! 	  then we need to read in new tables.

	  IF ( ( .NOT. ALLSTD ) .AND. ( ( IOGCE .NE. LOGCE ) .OR. ( IMTVL .NE. LMTVL ) ) )  IREADMT = 1

	ENDIF

	IF ( IREADMT .EQ. 0 ) RETURN

	LMT  = IMT
	LMTV = IMTV
	LOGCE = IOGCE
	LMTVL = IMTVL

	IF ( IPRT .GE. 2 ) THEN
        CALL ERRWRT(' ')
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	CALL ERRWRT('BUFRLIB: IREADMT - OPENING/READING MASTER TABLES')
	ENDIF

	IF ( ISC3(LUN) .NE. 0 ) THEN

! 	  Locate and open the master Table B files.  There should be one file of standard descriptors and one file of local
!	  descriptors.

	  CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, 'TableB', STDFIL, LOCFIL )
	  OPEN ( UNIT = LUN1, FILE = STDFIL, IOSTAT = IER )
	  IF ( IER .NE. 0 ) GOTO 900
	  OPEN ( UNIT = LUN2, FILE = LOCFIL, IOSTAT = IER )
	  IF ( IER .NE. 0 ) GOTO 901

! 	  Read the master Table B files.

	  CALL RDMTBB ( LUN1, LUN2, MXMTBB, IBMT, IBMTV, IBOGCE, IBLTV, NMTB, IBFXYN, CBSCL, CBSREF, CBBW, CBUNIT, &
                        CBMNEM, CMDSCB, CBELEM )

! 	  Close the master Table B files.

	  CLOSE ( UNIT = LUN1 )
	  CLOSE ( UNIT = LUN2 )

! 	  Locate and open the master Table D files.  There should be one file of standard descriptors and one file of local
!	  descriptors.

	  CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, 'TableD', STDFIL, LOCFIL )
	  OPEN ( UNIT = LUN1, FILE = STDFIL, IOSTAT = IER )
	  IF ( IER .NE. 0 ) GOTO 900
	  OPEN ( UNIT = LUN2, FILE = LOCFIL, IOSTAT = IER )
	  IF ( IER .NE. 0 ) GOTO 901

! 	  Read the master Table D files.

	  CALL RDMTBD ( LUN1, LUN2, MXMTBD, MAXCD, IDMT, IDMTV, IDOGCE, IDLTV, 	NMTD, IDFXYN, CDMNEM, CMDSCD, CDSEQ, &
      			NDELEM, IEFXYN, CEELEM )
	  DO I = 1, NMTD
	    DO J = 1, NDELEM(I)
	      IDX = ICVIDX ( I-1, J-1, MAXCD ) + 1
	      IDEFXY(IDX) = IEFXYN(I,J)
	    ENDDO
	  ENDDO

! 	  Close the master Table D files.

	  CLOSE ( UNIT = LUN1 )
	  CLOSE ( UNIT = LUN2 )

! 	  Copy master table B and D information into internal C arrays.

	  CALL CPMSTABS ( NMTB, IBFXYN, CBSCL, CBSREF, CBBW, CBUNIT, CBMNEM, CBELEM, NMTD, IDFXYN, CDSEQ, CDMNEM, &
      			  NDELEM, IDEFXY, MAXCD )
	ENDIF

	IF ( CDMF .EQ. 'Y' ) THEN

! 	  Locate and open the master code and flag table files.  There should be one file corresponding to the standard
!	  Table B descriptors, and one file corresponding to the local Table B descriptors.

	  CALL MTFNAM ( IMT, IMTV, IOGCE, IMTVL, 'CodeFlag', STDFIL, LOCFIL )
	  OPEN ( UNIT = LUN1, FILE = STDFIL, IOSTAT = IER )
	  IF ( IER .NE. 0 ) GOTO 900
	  OPEN ( UNIT = LUN2, FILE = LOCFIL, IOSTAT = IER )
	  IF ( IER .NE. 0 ) GOTO 901

! 	  Read the master code and flag table files.

	  CALL RDMTBF ( LUN1, LUN2 )

! 	  Close the master code and flag table files.

	  CLOSE ( UNIT = LUN1 )
	  CLOSE ( UNIT = LUN2 )

	ENDIF

	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF

	RETURN
900	BORT_STR = 'BUFRLIB: IREADMT - COULD NOT OPEN STANDARD FILE:'
	CALL BORT2(BORT_STR,STDFIL)
901	BORT_STR = 'BUFRLIB: IREADMT - COULD NOT OPEN LOCAL FILE:'
	CALL BORT2(BORT_STR,LOCFIL)

end
