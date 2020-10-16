C> @file
C> @author ATOR @date 2007-01-19
	
	SUBROUTINE RDMTBD ( LUNSTD, LUNLTD, MXMTBD, MXELEM,
     .			    IMT, IMTV, IOGCE, ILTV,
     .			    NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			    NMELEM, IEFXYN, CEELEM )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDMTBD
C   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19
C
C ABSTRACT:  THIS SUBROUTINE READS MASTER TABLE D INFORMATION FROM TWO
C   SEPARATE (I.E. ONE STANDARD AND ONE LOCAL) ASCII FILES AND THEN
C   MERGES IT INTO A UNIFIED SET OF MASTER TABLE D ARRAYS FOR OUTPUT.
C   EACH OF THE TWO INPUT FILES MUST ALREADY BE INDIVIDUALLY SORTED IN
C   ASCENDING ORDER WITH RESPECT TO THE FXY NUMBERS.
C
C PROGRAM HISTORY LOG:
C 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL RDMTBD ( LUNSTD, LUNLTD, MXMTBD, MXELEM,
C                         IMT, IMTV, IOGCE, ILTV,
C                         NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
C                         NMELEM, IEFXYN, CEELEM )
C   INPUT ARGUMENT LIST:
C     LUNSTD   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE
C                CONTAINING STANDARD TABLE D INFORMATION
C     LUNLTD   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE
C                CONTAINING LOCAL TABLE D INFORMATION
C     MXMTBD   - INTEGER: MAXIMUM NUMBER OF ENTRIES TO BE STORED IN
C                MERGED MASTER TABLE D ARRAYS; THIS SHOULD BE THE SAME
C                NUMBER AS WAS USED TO DIMENSION THE OUTPUT ARRAYS IN
C                THE CALLING PROGRAM, AND IT IS USED BY THIS SUBROUTINE
C                TO ENSURE THAT IT DOESN'T OVERFLOW THESE ARRAYS
C     MXELEM   - INTEGER: MAXIMUM NUMBER OF ELEMENTS TO BE STORED PER
C                ENTRY WITHIN THE MERGED MASTER TABLE D ARRAYS; THIS
C                SHOULD BE THE SAME NUMBER AS WAS USED TO DIMENSION THE
C                OUTPUT ARRAYS IN THE CALLING PROGRAM, AND IT IS USED
C                BY THIS SUBROUTINE TO ENSURE THAT IT DOESN'T OVERFLOW
C                THESE ARRAYS
C
C   OUTPUT ARGUMENT LIST:
C     IMT      - INTEGER: MASTER TABLE, READ FROM EACH ASCII FILE
C                (NOTE: THESE VALUES MUST BE THE SAME IN EACH FILE!)
C     IMTV     - INTEGER: VERSION NUMBER OF MASTER TABLE, READ FROM
C                STANDARD ASCII FILE
C     IOGCE    - INTEGER: ORIGINATING CENTER, READ FROM LOCAL ASCII FILE
C     ILTV     - INTEGER: VERSION NUMBER OF LOCAL TABLE, READ FROM
C                LOCAL ASCII FILE
C     NMTBD    - INTEGER: NUMBER OF ENTRIES IN MERGED MASTER TABLE D
C                ARRAYS
C     IMFXYN(*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE
C                REPRESENTATIONS OF FXY NUMBERS (I.E. SEQUENCE
C                DESCRIPTORS)
C     CMMNEM(*)- CHARACTER*8: MERGED ARRAY CONTAINING MNEMONICS
C     CMDSC(*) - CHARACTER*4: MERGED ARRAY CONTAINING DESCRIPTOR CODES
C     CMSEQ(*) - CHARACTER*120: MERGED ARRAY CONTAINING SEQUENCE NAMES
C     NMELEM(*)- INTEGER: MERGED ARRAY CONTAINING NUMBER OF ELEMENTS
C                STORED FOR EACH ENTRY
C   IEFXYN(*,*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE
C                REPRESENTATIONS OF ELEMENT FXY NUMBERS
C   CEELEM(*,*)- CHARACTER*120: MERGED ARRAY CONTAINING ELEMENT NAMES
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT     GETNTBE  GETTBH
C                               SNTBDE   WRDLEN
C    THIS ROUTINE IS CALLED BY: IREADMT
C                               Not normally called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	CHARACTER*200	STLINE, LTLINE
	CHARACTER*128	BORT_STR
	CHARACTER*120	CMSEQ(*), CEELEM(MXMTBD,MXELEM)
	CHARACTER*8	CMMNEM(*)
	CHARACTER*6	CMATCH, ADN30
	CHARACTER*4	CMDSC(*)

	INTEGER		IMFXYN(*), NMELEM(*),
     .			IEFXYN(MXMTBD,MXELEM)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Call WRDLEN to initialize some important information about the
C	local machine, just in case it hasn't already been called.

	CALL WRDLEN

C	Read and parse the header lines of both files.

	CALL GETTBH ( LUNSTD, LUNLTD, 'D', IMT, IMTV, IOGCE, ILTV )

C	Read through the remainder of both files, merging the
C	contents into a unified set of master Table D arrays.

	NMTBD = 0
	CALL GETNTBE ( LUNSTD, ISFXYN, STLINE, IERS )
	CALL GETNTBE ( LUNLTD, ILFXYN, LTLINE, IERL )
	DO WHILE ( ( IERS .EQ. 0 ) .OR. ( IERL .EQ. 0 ) )
	  IF ( ( IERS .EQ. 0 ) .AND. ( IERL .EQ. 0 ) ) THEN
	    IF ( ISFXYN .EQ. ILFXYN ) THEN
	      CMATCH = ADN30 ( ISFXYN, 6 )
	      GOTO 900
	    ELSE IF ( ISFXYN .LT. ILFXYN ) THEN
	      CALL SNTBDE ( LUNSTD, ISFXYN, STLINE, MXMTBD, MXELEM,
     .			    NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			    NMELEM, IEFXYN, CEELEM )
	      CALL GETNTBE ( LUNSTD, ISFXYN, STLINE, IERS )
	    ELSE
	      CALL SNTBDE ( LUNLTD, ILFXYN, LTLINE, MXMTBD, MXELEM,
     .			    NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			    NMELEM, IEFXYN, CEELEM )
	      CALL GETNTBE ( LUNLTD, ILFXYN, LTLINE, IERL )
	    ENDIF
	  ELSE IF ( IERS .EQ. 0 ) THEN
	    CALL SNTBDE ( LUNSTD, ISFXYN, STLINE, MXMTBD, MXELEM,
     .			  NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			  NMELEM, IEFXYN, CEELEM )
	    CALL GETNTBE ( LUNSTD, ISFXYN, STLINE, IERS )
	  ELSE IF ( IERL .EQ. 0 ) THEN
	    CALL SNTBDE ( LUNLTD, ILFXYN, LTLINE, MXMTBD, MXELEM,
     .			  NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			  NMELEM, IEFXYN, CEELEM )
	    CALL GETNTBE ( LUNLTD, ILFXYN, LTLINE, IERL )
	  ENDIF
	ENDDO

	RETURN
 900	WRITE(BORT_STR,'("BUFRLIB: RDMTBD - STANDARD AND LOCAL'//
     . ' TABLE D FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)')
     .	 CMATCH(1:1), '-', CMATCH(2:3), '-', CMATCH(4:6)	
	CALL BORT(BORT_STR)
	END
