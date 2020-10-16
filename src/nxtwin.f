C> @file
C> @author WOOLLEN @date 1994-01-06
      
      SUBROUTINE NXTWIN(LUN,IWIN,JWIN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NXTWIN
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: GIVEN INDICES WITHIN THE INTERNAL JUMP/LINK TABLE WHICH
C   POINT TO THE START AND END OF AN "RPC" WINDOW (I.E. ITERATION OF
C   AN 8-BIT OR 16-BIT DELAYED REPLICATION SEQUENCE), THIS SUBROUTINE
C   COMPUTES THE START AND END INDICES OF THE NEXT WINDOW.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY) (INCOMPLETE); OUTPUTS MORE
C                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2009-03-31  J. WOOLLEN -- ADDED ADDITIONAL DOCUMENTATION
C 2009-05-07  J. ATOR    -- USE LSTJPB INSTEAD OF LSTRPC
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL NXTWIN (LUN, IWIN, JWIN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     IWIN     - INTEGER: STARTING INDEX OF CURRENT WINDOW ITERATION
C     JWIN     - INTEGER: ENDING INDEX OF CURRENT WINDOW ITERATION
C
C   OUTPUT ARGUMENT LIST:
C     IWIN     - INTEGER: STARTING INDEX OF NEXT WINDOW ITERATION
C     JWIN     - INTEGER: ENDING INDEX OF NEXT WINDOW ITERATION
C
C REMARKS:
C
C    SEE THE DOCBLOCK IN BUFR ARCHIVE LIBRARY SUBROUTINE GETWIN FOR AN
C    EXPLANATION OF "WINDOWS" WITHIN THE CONTEXT OF A BUFR DATA SUBSET.
C
C    THIS ROUTINE CALLS:        BORT     LSTJPB
C    THIS ROUTINE IS CALLED BY: UFBEVN   UFBIN3   UFBRW
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_USRINT

      INCLUDE 'bufrlib.prm'

      CHARACTER*128 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(JWIN.EQ.NVAL(LUN)) THEN
         IWIN = 0
         GOTO 100
      ENDIF

C  FIND THE NEXT SEQUENTIAL WINDOW
C  -------------------------------

      NODE = INV(IWIN,LUN)
      IF(LSTJPB(NODE,LUN,'RPC').NE.NODE) GOTO 900
      IF(VAL(JWIN,LUN).EQ.0) THEN
         IWIN = 0
      ELSE
         IWIN = JWIN
         JWIN = IWIN+VAL(IWIN,LUN)
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NXTWIN - LSTJPB FOR NODE",I6," '//
     . '(LSTJPB=",I5,") DOES NOT EQUAL VALUE OF NODE, NOT RPC (IWIN '//
     . '=",I8,")")') NODE,LSTJPB(NODE,LUN,'RPC'),IWIN
      CALL BORT(BORT_STR)
      END
