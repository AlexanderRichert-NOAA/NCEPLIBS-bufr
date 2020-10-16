C> @file
C> @author J @date 2009-03-23
      
      SUBROUTINE WRDXTB(LUNDX,LUNOT)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRDXTB
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: THIS SUBROUTINE WRITES BUFR TABLE (DICTIONARY) MESSAGES
C   ASSOCIATED WITH THE BUFR FILE IN LUNDX TO THE BUFR FILE IN LUNOT.
C   BOTH UNITS MUST BE OPENED VIA PREVIOUS CALLS TO BUFR ARCHIVE
C   LIBRARY SUBROUTINE OPENBF, AND IN PARTICULAR LUNOT MUST HAVE
C   BEEN OPENED FOR OUTPUT.  THE TABLE MESSAGES ARE GENERATED FROM
C   ARRAYS IN INTERNAL MEMORY (MODULE TABABD).  LUNDX CAN BE THE
C   SAME AS LUNOT IF IT IS DESIRED TO APPEND TO LUNOT WITH BUFR
C   MESSAGES GENERATED FROM ITS OWN INTERNAL TABLES.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR, USING LOGIC FROM WRITDX
C 2012-04-06  J. ATOR    -- PREVENT STORING OF MORE THAN 255 TABLE A,
C                           TABLE B OR TABLE D DESCRIPTORS IN ANY
C                           SINGLE DX MESSAGE
C 2014-11-14  J. ATOR    -- REPLACE IPKM CALLS WITH PKB CALLS
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL WRDXTB (LUNDX,LUNOT)
C   INPUT ARGUMENT LIST:
C     LUNDX    - INTEGER: FORTRAN LOGICAL UNIT NUMBER ASSOCIATED
C                WITH DX (DICTIONARY) TABLES TO BE WRITTEN OUT;
C                CAN BE SAME AS LUNOT
C     LUNOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C                TO BE APPENDED WITH TABLES ASSOCIATED WITH LUNDX
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT     CPBFDX   DXMINI
C                               GETLENS  IUPB     IUPM     MSGFULL
C                               MSGWRT   PKB      PKC      STATUS
C    THIS ROUTINE IS CALLED BY: MAKESTAB WRITDX
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_TABABD
      USE MODA_MGWA

      INCLUDE 'bufrlib.prm'

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)

      CHARACTER*128 BORT_STR
      CHARACTER*56  DXSTR
      CHARACTER*6   ADN30

      LOGICAL MSGFULL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FILE STATUSES
C  -------------------

      CALL STATUS(LUNOT,LOT,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901

      CALL STATUS(LUNDX,LDX,IL,IM)
      IF(IL.EQ.0) GOTO 902

C  IF FILES ARE DIFFERENT, COPY INTERNAL TABLE
C  INFORMATION FROM LUNDX TO LUNOT
C  -------------------------------------------

      IF(LUNDX.NE.LUNOT) CALL CPBFDX(LDX,LOT)

C  GENERATE AND WRITE OUT BUFR DICTIONARY MESSAGES TO LUNOT
C  --------------------------------------------------------

      CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)

      LDA = LDXA(IDXV+1)
      LDB = LDXB(IDXV+1)
      LDD = LDXD(IDXV+1)
      L30 = LD30(IDXV+1)

C     Table A information

      DO I=1,NTBA(LOT)
      IF(MSGFULL(MBYT,LDA,MAXDX).OR.
     +    (IUPB(MGWA,MBYA,8).EQ.255)) THEN
         CALL MSGWRT(LUNOT,MGWA,MBYT)
         CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      ENDIF
      MBIT = 8*(MBY4-1)
      CALL PKB(IUPB(MGWA,MBY4,24)+LDA,24,MGWA,MBIT)
      MBIT = 8*(MBYA-1)
      CALL PKB(IUPB(MGWA,MBYA, 8)+  1, 8,MGWA,MBIT)
      MBIT = 8*(MBYB-1)
      CALL PKC(TABA(I,LOT),LDA,MGWA,MBIT)
      CALL PKB(          0,  8,MGWA,MBIT)
      CALL PKB(          0,  8,MGWA,MBIT)
      MBYT = MBYT+LDA
      MBYB = MBYB+LDA
      MBYD = MBYD+LDA
      ENDDO

C     Table B information

      DO I=1,NTBB(LOT)
      IF(MSGFULL(MBYT,LDB,MAXDX).OR.
     +    (IUPB(MGWA,MBYB,8).EQ.255)) THEN
         CALL MSGWRT(LUNOT,MGWA,MBYT)
         CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      ENDIF
      MBIT = 8*(MBY4-1)
      CALL PKB(IUPB(MGWA,MBY4,24)+LDB,24,MGWA,MBIT)
      MBIT = 8*(MBYB-1)
      CALL PKB(IUPB(MGWA,MBYB, 8)+  1, 8,MGWA,MBIT)
      MBIT = 8*(MBYD-1)
      CALL PKC(TABB(I,LOT),LDB,MGWA,MBIT)
      CALL PKB(          0,  8,MGWA,MBIT)
      MBYT = MBYT+LDB
      MBYD = MBYD+LDB
      ENDDO

C     Table D information

      DO I=1,NTBD(LOT)
      NSEQ = IUPM(TABD(I,LOT)(LDD+1:LDD+1),8)
      LEND = LDD+1 + L30*NSEQ
      IF(MSGFULL(MBYT,LEND,MAXDX).OR.
     +    (IUPB(MGWA,MBYD,8).EQ.255)) THEN
         CALL MSGWRT(LUNOT,MGWA,MBYT)
         CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      ENDIF
      MBIT = 8*(MBY4-1)
      CALL PKB(IUPB(MGWA,MBY4,24)+LEND,24,MGWA,MBIT)
      MBIT = 8*(MBYD-1)
      CALL PKB(IUPB(MGWA,MBYD, 8)+   1, 8,MGWA,MBIT)
      MBIT = 8*(MBYT-4)
      CALL PKC(TABD(I,LOT),LDD,MGWA,MBIT)
      CALL PKB(       NSEQ,  8,MGWA,MBIT)
         DO J=1,NSEQ
         JJ  = LDD+2 + (J-1)*2
         IDN = IUPM(TABD(I,LOT)(JJ:JJ),16)
         CALL PKC(ADN30(IDN,L30),L30,MGWA,MBIT)
         ENDDO
      MBYT = MBYT+LEND
      ENDDO

C     Write the unwritten (leftover) message.

      CALL MSGWRT(LUNOT,MGWA,MBYT)

C     Write out one additional (dummy) DX message containing zero
C     subsets.  This will serve as a delimiter for this set of
C     table messages within output unit LUNOT, just in case the
C     next thing written to LUNOT ends up being another set of
C     table messages.

      CALL DXMINI(LOT,MGWA,MBYT,MBY4,MBYA,MBYB,MBYD)
      CALL GETLENS(MGWA,2,LEN0,LEN1,LEN2,L3,L4,L5)
      MBIT = (LEN0+LEN1+LEN2+4)*8
      CALL PKB(0,16,MGWA,MBIT)
      CALL MSGWRT(LUNOT,MGWA,MBYT)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: WRDXTB - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRDXTB - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRDXTB - DX TABLE FILE IS CLOSED, IT '//
     . 'MUST BE OPEN')
      END
