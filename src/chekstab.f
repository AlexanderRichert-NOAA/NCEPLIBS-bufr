C> @file
C> @author WOOLLEN @date 1994-01-06
      
      SUBROUTINE CHEKSTAB(LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CHEKSTAB
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE CHECKS THAT AN INTERNAL BUFR TABLE
C   REPRESENTATION IS SELF-CONSISTENT AND FULLY DEFINED.  IF ANY ERRORS
C   ARE FOUND, THEN AN APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY
C   SUBROUTINE BORT.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL CHEKSTAB (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     NEMTAB   NEMTBB   NEMTBD
C    THIS ROUTINE IS CALLED BY: MAKESTAB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_TABABD
      USE MODA_NMIKRP

      INCLUDE 'bufrlib.prm'

      CHARACTER*128 BORT_STR
      CHARACTER*24  UNIT
      CHARACTER*8   NEMO
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  THERE MUST BE ENTRIES IN TABLES A, B, AND D
C  -------------------------------------------

      IF(NTBA(LUN).EQ.0) GOTO 900
      IF(NTBB(LUN).EQ.0) GOTO 901
      IF(NTBD(LUN).EQ.0) GOTO 902

C  MAKE SURE EACH TABLE A ENTRY DEFINED AS A SEQUENCE
C  --------------------------------------------------

      DO I=1,NTBA(LUN)
      NEMO = TABA(I,LUN)(4:11)
      CALL NEMTAB(LUN,NEMO,IDN,TAB,IRET)
      IF(TAB.NE.'D') GOTO 903
      ENDDO

C  CHECK TABLE B CONTENTS
C  ----------------------

      DO ITAB=1,NTBB(LUN)
      CALL NEMTBB(LUN,ITAB,UNIT,ISCL,IREF,IBIT)
      ENDDO

C  CHECK TABLE D CONTNETS
C  ----------------------

      DO ITAB=1,NTBD(LUN)
      CALL NEMTBD(LUN,ITAB,NSEQ,NEM(1,1),IRP(1,1),KRP(1,1))
      ENDDO

C  EXITS
C  -----

      RETURN
900   CALL BORT
     . ('BUFRLIB: CHEKSTAB - EMPTY TABLE A IN INTERNAL BUFR TABLES')
901   CALL BORT
     . ('BUFRLIB: CHEKSTAB - EMPTY TABLE B IN INTERNAL BUFR TABLES')
902   CALL BORT
     . ('BUFRLIB: CHEKSTAB - EMPTY TABLE D IN INTERNAL BUFR TABLES')
903   WRITE(BORT_STR,'("BUFRLIB: CHEKSTAB - TABLE A ENTRY: ",A," NOT '//
     . 'DEFINED AS A SEQUENCE")') NEMO
      CALL BORT(BORT_STR)
      END
