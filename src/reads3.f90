!> @file
!> @author ATOR @date 2009-03-23
	
!> THIS SUBROUTINE READS THE SECTION 3 DESCRIPTORS FROM THE
!>   BUFR MESSAGE IN MBAY(:,LUN).  IT THEN USES THE BUFR MASTER TABLES
!>   TO GENERATE THE NECESSARY INFORMATION FOR THESE DESCRIPTORS WITHIN
!>   THE INTERNAL BUFR TABLE ARRAYS.
!>
!> PROGRAM HISTORY LOG:
!> 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
!> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
!> 2017-10-13  J. ATOR    -- REMOVE FUNCTIONALITY TO CHECK WHETHER NEW
!>                           MASTER TABLES NEED TO BE READ (THIS
!>                           FUNCTIONALITY IS NOW PART OF FUNCTION
!>                           IREADMT)
!>
!> USAGE:    CALL READS3 ( LUN )
!>   INPUT ARGUMENT LIST:
!>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        ADN30    BORT     DXINIT   ERRWRT
!>                               IFXY     IGETNTBI IGETTDI  IREADMT
!>                               MAKESTAB STNTBIA  STSEQ    UPDS3
!>    THIS ROUTINE IS CALLED BY: READERME READMG
!>                               Normally not called by any application
!>                               programs.
!>

subroutine reads3 ( LUN )

	USE MODA_SC3BFR
	USE MODA_BITBUF
        USE MODA_DSCACH

        use subroutine_upds3

	COMMON /QUIET/  IPRT

	DIMENSION	IDS3(MAXNC)
	CHARACTER*6	CDS3(MAXNC),NUMB,ADN30

	CHARACTER*55	CSEQ

	CHARACTER*128	ERRSTR

	LOGICAL		INCACH

	SAVE	IREPCT

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!	Check whether the appropriate BUFR master table information has already been read into internal memory for this message.

	IF ( IREADMT ( LUN ) .EQ. 1 ) THEN

!	  NO (i.e. we just had to read in new master table information for this message), so reset some corresponding values in
!	  other parts of the library.

	  CALL DXINIT ( LUN, 0 )
	  ITMP = IGETTDI ( 0 )
	  IREPCT = 0
	  NCNEM = 0
	ENDIF

!	Unpack the list of Section 3 descriptors from the message.

	CALL UPDS3 ( MBAY(:,LUN), MAXNC, CDS3, NCDS3 )
	DO II = 1, NCDS3
	  IDS3(II) = IFXY( CDS3(II) )
	ENDDO

!	Is the list of Section 3 descriptors already in the cache?

!	The cache is a performance-enhancing device which saves time when the same descriptor sequences are encountered over and
!	over within the calling program.  Time is saved because the below calls to subroutines STSEQ and MAKESTAB are bypassed
!	whenever a list is already in the cache.

	INCACH = .FALSE.
	IF ( NCNEM .GT. 0 ) THEN
	  II = 1
	  DO WHILE ( (.NOT.INCACH) .AND. (II.LE.NCNEM) )
	    IF ( NCDS3 .EQ. NDC(II) ) THEN
	      JJ = 1
	      INCACH = .TRUE.
	      DO WHILE ( (INCACH) .AND. (JJ.LE.NCDS3) )
		IF ( IDS3(JJ) .EQ. IDCACH(II,JJ) ) THEN
		  JJ = JJ + 1
		ELSE
		  INCACH = .FALSE.
		ENDIF
	      ENDDO
	      IF (INCACH) THEN

!		The list is already in the cache, so store the
!		corresponding Table A mnemonic into MODULE SC3BFR
!		and return.

		IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	ERRSTR = 'BUFRLIB: READS3 - RE-USED CACHE LIST FOR ' // CNEM(II)
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
		ENDIF
		TAMNEM(LUN) = CNEM(II)
		RETURN
	      ENDIF
	    ENDIF
	    II = II + 1
	  ENDDO
	ENDIF

!	Get the next available index within the internal Table A.

	N = IGETNTBI ( LUN, 'A' )

!	Generate a Table A mnemonic and sequence description.

	WRITE ( TAMNEM(LUN), '(A5,I3.3)') 'MSTTB', N
	CSEQ = 'TABLE A MNEMONIC ' // TAMNEM(LUN)

!	Store the Table A mnemonic and sequence into the cache.

	NCNEM = NCNEM + 1
	IF ( NCNEM .GT. MXCNEM ) GOTO 900
	CNEM(NCNEM) = TAMNEM(LUN)
	NDC(NCNEM) = NCDS3
	DO JJ = 1, NCDS3
	  IDCACH(NCNEM,JJ) = IDS3(JJ)
	ENDDO
	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	ERRSTR = 'BUFRLIB: READS3 - STORED CACHE LIST FOR ' // CNEM(NCNEM)
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF

!	Get an FXY value to use with this Table A mnemonic.
  
	IDN = IGETTDI ( LUN )
	NUMB = ADN30 ( IDN, 6 )

!	Store all of the information for this mnemonic within the
!	internal Table A.

	CALL STNTBIA ( N, LUN, NUMB, TAMNEM(LUN), CSEQ )

!	Store all of the information for this sequence within the
!	internal Tables B and D.

	CALL STSEQ ( LUN, IREPCT, IDN, TAMNEM(LUN), CSEQ, IDS3, NCDS3 )

!	Update the jump/link table.

	CALL MAKESTAB

	RETURN
900	CALL BORT('BUFRLIB: READS3 - MXCNEM OVERFLOW')

end
