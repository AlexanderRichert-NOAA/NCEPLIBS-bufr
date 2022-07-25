C> @file
C> @author WOOLLEN @date 1996-12-11
      
C> THIS SUBROUTINE RETURNS THE SECTION 1 DATE IN THE FIRST
C>   TWO NON-DICTIONARY BUFR MESSAGES IN LOGICAL UNIT LUNIT WHICH
C>   CONTAIN ZERO SUBSETS.  NORMALLY, THESE "DUMMY" MESSAGES APPEAR
C>   ONLY IN DATA DUMP FILES AND ARE IMMEDIATELY AFTER THE DICTIONARY
C>   MESSAGES.  THEY CONTAIN A DUMP "CENTER TIME" AND A DUMP FILE
C>   "PROCESSING TIME", RESPECTIVELY.  LUNIT SHOULD NOT BE PREVIOUSLY
C>   OPENED TO THE BUFR INTERFACE.
C>
C> PROGRAM HISTORY LOG:
C> 1996-12-11  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1996-12-17  J. WOOLLEN -- CORRECTED ERROR IN DUMP DATE READER
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K
C>                           COMPLIANT
C> 2003-05-19  M. SHIREY  -- REPLACED CALLS TO FORTRAN INSRINSIC
C>                           FUNCTION ICHAR WITH THE NCEP W3LIB C-
C>                           FUNCTION MOVA2I BECAUSE ICHAR DOES NOT WORK 
C>                           PROPERLY ON SOME MACHINES (E.G., IBM FROST/
C>                           SNOW) (NOTE: ON 2003-??-??, MOVA2I WAS
C>                           ADDED TO THE BUFRLIB AS A FORTRAN FUNCTION)
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MODIFIED DATE CALCULATIONS TO NO LONGER
C>                           USE FLOATING POINT ARITHMETIC SINCE THIS
C>                           CAN LEAD TO ROUND OFF ERROR AND AN IMPROPER
C>                           RESULTING DATE ON SOME MACHINES (E.G., NCEP
C>                           IBM FROST/SNOW), INCREASES PORTABILITY;
C>                           UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY OR UNUSUAL THINGS
C>                           HAPPEN
C> 2004-08-18  J. ATOR    -- MODIFIED 'BUFR' STRING TEST FOR PORTABILITY
C>                           TO EBCDIC MACHINES
C> 2004-12-20  D. KEYSER  -- CALLS WRDLEN TO INITIALIZE LOCAL MACHINE
C>                           INFORMATION (IN CASE IT HAS NOT YET BEEN
C>                           CALLED), THIS ROUTINE DOES NOT REQUIRE IT
C>                           BUT 2004-08-18 CHANGE CALLS OTHER ROUTINES
C>                           THAT DO REQUIRE IT
C> 2005-11-29  J. ATOR    -- USE IUPBS01, IGETDATE, GETLENS AND RDMSGW
C> 2009-03-23  J. ATOR    -- USE IDXMSG, IUPBS3 AND ERRWRT
C> 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C>                           USE NEW OPENBF TYPE 'INX' TO OPEN AND CLOSE
C>                           THE C FILE WITHOUT CLOSING THE FORTRAN FILE
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL DUMPBF (LUNIT, JDATE, JDUMP)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>
C>   OUTPUT ARGUMENT LIST:
C>     JDATE    - INTEGER: 5-WORD ARRAY CONTAINING THE YEAR
C>                (YYYY OR YY, DEPENDING ON DATELEN() VALUE),
C>                MONTH, DAY, HOUR AND MINUTE FROM SECTION 1 OF THE
C>                FIRST NON-DICTIONARY BUFR MESSAGE WITH ZERO SUBSETS
C>                (NORMALLY THE DATA DUMP CENTER TIME IN A DATA DUMP
C>                FILE); OR 5*-1 IF THIS COULD NOT BE LOCATED
C>     JDUMP    - INTEGER: 5-WORD ARRAY CONTAINING THE YEAR
C>                (YYYY OR YY, DEPENDING ON DATELEN() VALUE),
C>                MONTH, DAY, HOUR AND MINUTE FROM SECTION 1 OF THE
C>                SECOND NON-DICTIONARY BUFR MESSAGE WITH ZERO SUBSETS
C>                (NORMALLY THE FILE PROCESSING TIME IN A DATA DUMP
C>                FILE); OR 5*-1 IF THIS COULD NOT BE LOCATED
C>
C>   INPUT FILES:
C>     UNIT "LUNIT" - BUFR FILE
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     ERRWRT   IDXMSG   IGETDATE
C>                               IUPBS01  IUPBS3   RDMSGW   STATUS
C>                               WRDLEN
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------
      SUBROUTINE DUMPBF_8(LUNIT_8,JDATE_8,JDUMP_8)
      INTEGER*8  LUNIT_8,JDATE_8(5),JDUMP_8(5)
      INTEGER    LUNIT  ,JDATE(5)  ,JDUMP(5)
      LUNIT=LUNIT_8
      JDATE=JDATE_8
      JDUMP=JDUMP_8  
      CALL DUMPBF(LUNIT,JDATE,JDUMP)
      JDATE_8=JDATE
      JDUMP_8=JDUMP
      END SUBROUTINE
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------

      SUBROUTINE DUMPBF(LUNIT,JDATE,JDUMP)

      USE MODA_MGWA
      USE MODA_IM8B

      COMMON /QUIET / IPRT

      DIMENSION     JDATE(5),JDUMP(5)

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------
      IF(IM8) THEN
         IM8=.FALSE.
         CALL DUMPBF_8(LUNIT,JDATE,JDUMP)
         IM8=.TRUE.
         RETURN
      ENDIF

C  CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION
C  ABOUT THE LOCAL MACHINE (IN CASE IT HAS NOT YET BEEN CALLED)
C  ---------------------------------------------------------------

      CALL WRDLEN

      DO I=1,5
        JDATE(I) = -1
        JDUMP(I) = -1
      ENDDO

C  SEE IF THE FILE IS ALREADY OPEN TO BUFR INTERFACE (A NO-NO)
C  -----------------------------------------------------------

      CALL STATUS(LUNIT,LUN,JL,JM)
      IF(JL.NE.0) GOTO 900
      call openbf(lunit,'INX',lunit)

C  READ PAST ANY DICTIONARY MESSAGES
C  ---------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.LT.0) GOTO 200
      IF(IDXMSG(MGWA).EQ.1) GOTO 1

C  DUMP CENTER YY,MM,DD,HH,MM IS IN THE FIRST EMPTY MESSAGE
C  --------------------------------------------------------
C  i.e. the first message containing zero subsets
     
      IF(IUPBS3(MGWA,'NSUB').NE.0) GOTO 200

      IGD = IGETDATE(MGWA,JDATE(1),JDATE(2),JDATE(3),JDATE(4))
      JDATE(5) = IUPBS01(MGWA,'MINU')

C  DUMP CLOCK YY,MM,DD,HH,MM IS IN THE SECOND EMPTY MESSAGE
C  --------------------------------------------------------
C  i.e. the second message containing zero subsets

      CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.LT.0) GOTO 200
     
      IF(IUPBS3(MGWA,'NSUB').NE.0) GOTO 200

      IGD = IGETDATE(MGWA,JDUMP(1),JDUMP(2),JDUMP(3),JDUMP(4))
      JDUMP(5) = IUPBS01(MGWA,'MINU')

      call closbf(lunit)
      GOTO 100

200   IF(IPRT.GE.1 .AND. (JDATE(1).EQ.-1.OR.JDUMP(1).EQ.-1)) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      IF(JDATE(1).EQ.-1) THEN
        ERRSTR = 'BUFRLIB: DUMPBF - FIRST  EMPTY BUFR MESSAGE '//
     .    'SECTION 1 DATE COULD NOT BE LOCATED - RETURN WITH '//
     .    'JDATE = 4*-1'
        CALL ERRWRT(ERRSTR)
      ENDIF
      IF(JDUMP(1).EQ.-1) THEN
        ERRSTR = 'BUFRLIB: DUMPBF - SECOND EMPTY BUFR MESSAGE '//
     .    'SECTION 1 DATE COULD NOT BE LOCATED - RETURN WITH '//
     .    'JDUMP = 4*-1'
        CALL ERRWRT(ERRSTR)
      ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT
     . ('BUFRLIB: DUMPBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')
      END
