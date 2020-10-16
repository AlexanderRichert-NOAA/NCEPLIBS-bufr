C> @file
C> @author WOOLLEN @date 2003-11-04
      
      SUBROUTINE WRITLC(LUNIT,CHR,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRITLC
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04
C
C ABSTRACT: THIS SUBROUTINE PACKS A CHARACTER DATA ELEMENT ASSOCIATED
C   WITH A PARTICULAR SUBSET MNEMONIC FROM THE INTERNAL MESSAGE BUFFER
C   (ARRAY MBAY IN MODULE BITBUF).  IT IS DESIGNED TO BE USED
C   TO STORE CHARACTER ELEMENTS GREATER THAN THE USUAL LENGTH OF EIGHT
C   BYTES.  NOTE THAT SUBROUTINE WRITSB OR WRITSA MUST HAVE ALREADY
C   BEEN CALLED TO STORE ALL OTHER ELEMENTS OF THE SUBSET BEFORE THIS
C   SUBROUTINE CAN BE CALLED TO FILL IN ANY LONG CHARACTER STRINGS.
C
C PROGRAM HISTORY LOG:
C 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-11-29  J. ATOR    -- USE GETLENS
C 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR
C 2009-03-23  J. ATOR    -- ADDED '#' OPTION FOR MORE THAN ONE
C                           OCCURRENCE OF STR 
c 2009-08-11  J. WOOLLEN -- ADDED COMMON COMPRS ALONG WITH LOGIC TO 
c                           WRITE LONG STRINGS INTO COMPRESSED SUBSETS
C 2012-12-07  J. ATOR    -- ALLOW STR MNEMONIC LENGTH OF UP TO 14 CHARS
C                           WHEN USED WITH '#' OCCURRENCE CODE
C 2014-10-22  J. ATOR    -- NO LONGER ABORT IF NO SUBSET AVAILABLE FOR
C                           WRITING; JUST PRINT A WARNING MESSAGE
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL WRITLC (LUNIT, CHR, STR)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     CHR      - CHARACTER*(*): UNPACKED CHARACTER STRING (I.E.,
C                CHARACTER DATA ELEMENT GREATER THAN EIGHT BYTES)
C     STR      - CHARACTER*(*): MNEMONIC ASSOCIATED WITH STRING IN CHR
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     GETLENS  IUPBS3   PARSTR
C                               PARUTG   PKC      STATUS   UPB
C                               UPBB     USRTPL
C    THIS ROUTINE IS CALLED BY: MSGUPD
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODA_COMPRS

      INCLUDE 'bufrlib.prm'

      COMMON /QUIET / IPRT

      CHARACTER*(*) CHR,STR
      CHARACTER*128 BORT_STR
      CHARACTER*128 ERRSTR
      CHARACTER*10  CTAG
      CHARACTER*14  TGS(10)

      DATA MAXTG /10/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  CHECK FOR TAGS (MNEMONICS) IN INPUT STRING (THERE CAN ONLY BE ONE)
C  ------------------------------------------------------------------

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      IF(NTG.GT.1) GOTO 903

C     Check if a specific occurrence of the input string was requested;
C     if not, then the default is to write the first occurrence.

      CALL PARUTG(LUN,1,TGS(1),NNOD,KON,ROID)
      IF(KON.EQ.6) THEN
         IOID=NINT(ROID)
         IF(IOID.LE.0) IOID = 1
         CTAG = ' '
         II = 1
         DO WHILE((II.LE.10).AND.(TGS(1)(II:II).NE.'#'))
            CTAG(II:II)=TGS(1)(II:II)
            II = II + 1
         ENDDO
      ELSE
         IOID = 1
         CTAG = TGS(1)(1:10)
      ENDIF

C  USE THIS LEG FOR STRINGING COMPRESSED DATA (UP TO MXLCC CHARACTERS)
C  ----------------------------------------------------------------

      IF(IUPBS3(MBAY(1,LUN),'ICMP').GT.0) THEN       
         N = 1
         ITAGCT = 0
         CALL USRTPL(LUN,N,N)
         DO WHILE (N+1.LE.NVAL(LUN))
            N = N+1
            NODE = INV(N,LUN)
            IF(ITP(NODE).EQ.1) THEN
               CALL USRTPL(LUN,N,MATX(N,NCOL))
            ELSEIF(CTAG.EQ.TAG(NODE)) THEN
               ITAGCT = ITAGCT + 1
               IF(ITAGCT.EQ.IOID) THEN 
                  IF(ITP(NODE).NE.3) GOTO 904
                  CATX(N,NCOL)=' '
C                 --------------------------------------------------
C                 Note: the following stmt enforces a limit of MXLCC 
C                 characters per long character string when writing
C                 compressed messages.  This limit keeps the static
C                 array CATX to a reasonable dimensioned size. 
C                 --------------------------------------------------
                  NCHR=MIN(MXLCC,IBT(NODE)/8)
                  CATX(N,NCOL)=CHR(1:NCHR)
                  CALL USRTPL(LUN,1,1)
                  GOTO 100
               ENDIF
            ENDIF
         ENDDO
         GOTO 906
      ENDIF

C  OTHERWISE LOCATE THE BEGINNING OF THE DATA (SECTION 4) IN THE MESSAGE
C  ---------------------------------------------------------------------

      CALL GETLENS(MBAY(1,LUN),3,LEN0,LEN1,LEN2,LEN3,L4,L5)
      MBYTE = LEN0 + LEN1 + LEN2 + LEN3 + 4
      NSUBS = 1

C  FIND THE MOST RECENTLY WRITTEN SUBSET IN THE MESSAGE
C  ----------------------------------------------------

      DO WHILE(NSUBS.LT.NSUB(LUN))
         IBIT = MBYTE*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         MBYTE = MBYTE + NBYT
         NSUBS = NSUBS + 1
      ENDDO

      IF(NSUBS.NE.NSUB(LUN)) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: WRITLC - COULD NOT WRITE VALUE FOR ' // CTAG
     . // ' INTO SUBSET, BECAUSE NO SUBSET WAS OPEN FOR WRITING'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
         CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ENDIF

C  LOCATE AND WRITE THE LONG CHARACTER STRING WITHIN THIS SUBSET
C  -------------------------------------------------------------

      ITAGCT = 0
      MBIT = MBYTE*8 + 16
      NBIT = 0
      N = 1
      CALL USRTPL(LUN,N,N)
      DO WHILE (N+1.LE.NVAL(LUN))
         N = N+1
         NODE = INV(N,LUN)
         MBIT = MBIT+NBIT
         NBIT = IBT(NODE)
         IF(ITP(NODE).EQ.1) THEN
            CALL UPBB(IVAL,NBIT,MBIT,MBAY(1,LUN))
            CALL USRTPL(LUN,N,IVAL)
         ELSEIF(CTAG.EQ.TAG(NODE)) THEN
            ITAGCT = ITAGCT + 1
            IF(ITAGCT.EQ.IOID) THEN 
              IF(ITP(NODE).NE.3) GOTO 904
              NCHR = NBIT/8
              IBIT = MBIT
              DO J=1,NCHR
                CALL PKC(' ',1,MBAY(1,LUN),IBIT)
              ENDDO
              CALL PKC(CHR,NCHR,MBAY(1,LUN),MBIT)
              CALL USRTPL(LUN,1,1)
              GOTO 100
            ENDIF
         ENDIF
      ENDDO
      GOTO 906

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN '//
     . ' ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4'//
     . ',")")') STR,NTG
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT '//
     . 'REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') TGS(1),TYP(NODE)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THE MOST RECENTLY WRITTEN '//
     . ' SUBSET NO. (",I3,") IN MSG .NE. THE STORED VALUE FOR THE NO.'//
     . ' OF SUBSETS (",I3,") IN MSG")') NSUBS,NSUB(LUN)
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLB: WRITLC - UNABLE TO FIND ",A," IN '//
     . 'SUBSET")') TGS(1)
      CALL BORT(BORT_STR)
      END
