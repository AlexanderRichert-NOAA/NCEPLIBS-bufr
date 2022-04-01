!> @file
!> @brief Write a long character string (greater than 8 bytes) to a data subset.

!> This subroutine writes a long character string (greater than 8 bytes) to a data subset.
!>
!> <p>The data subset should have already been written into a BUFR message via a previous call to one of the
!> [subset-writing subroutines](@ref hierarchy), before calling this subroutine to write any long character strings
!> into the same subset.
!>
!> @authors J. Woollen
!> @authors J. Ator
!> @date 2003-11-04
!>
!> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
!> @param[in] CHR  -- character*(*): Value corresponding to STR
!> @param[in] STR   -- character*(*): Table B mnemonic of long character string to be written, possibly supplemented
!>                     with an ordinal occurrence notation
!>
!> <p>If there is more than one occurrence of STR within the data subset definition, then each occurrence can be written
!> via a separate call to this subroutine, and by appending the ordinal number of the occurrence to STR in each case.
!> For example, if there are 5 occurrences of mnemonic LSTID within a given data subset definition, then 5 separate calls
!> should be made to this subroutine, once each with STR set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and 'LSTID#5'.
!> However, the first notation is superfluous, because omitting the ordinal number always defaults to the first occurrence
!> of a particular string, so a user could just specify 'LSTID' instead of 'LSTID#1'.
!>
!> @remarks
!> - Character strings which are 8 bytes or less in length can be written by converting the string into a real*8 value within
!> the application program, and then using the real*8 USR array within a call to one of the BUFRLIB
!> [values-writing subroutines](@ref hierarchy) prior to calling one of the [subset-writing subroutines](@ref hierarchy)
!> for the data subset.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2003-11-04 | J. Woollen | Original author |
!> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-11-29 | J. Ator    | Use getlens() |
!> | 2007-01-19 | J. Ator    | Replaced call to parseq with call to parstr() |
!> | 2009-03-23 | J. Ator    | Added '#' option for more than one occurrence of STR |
!> | 2009-08-11 | J. Woollen | Added COMMON COMPRS along with logic to write long strings into compressed subsets |
!> | 2012-12-07 | J. Ator    | Allow str mnemonic length of up to 14 chars when used with '#' occurrence code |
!> | 2014-10-22 | J. Ator    | No longer abort if no subset available for writing; just print a warning message |
!> | 2014-12-10 | J. Ator    | USE modules instead of COMMON blocks |
!> | 2020-09-09 | J. Ator    | No longer abort if STR not available within subset definition; instead, just print a warning message |
!> | 2022-02-01 | J. Ator    | Convert to Fortran90 |
!>

subroutine writlc(LUNIT,CHR,STR)

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODA_COMPRS

      use subroutine_getlens
      use function_iupbs3

      COMMON /QUIET / IPRT

      CHARACTER*(*) CHR,STR
      CHARACTER*128 BORT_STR
      CHARACTER*128 ERRSTR
      CHARACTER*10  CTAG
      CHARACTER*14  TGS(10)

      DATA MAXTG /10/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!     Check the file status.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

!     Check for tags (mnemonics) in input string (there can only be one)

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      IF(NTG.GT.1) GOTO 903

!     Check if a specific occurrence of the input string was requested; if not, then the default is to write the first occurrence.

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

      IF(IUPBS3(MBAY(:,LUN),'ICMP').GT.0) THEN       

!        The message is compressed.

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

!                 The following statement enforces a limit of MXLCC characters per long character string when writing
!                 compressed messages.  This limit keeps the array CATX to a reasonable dimensioned size. 

                  NCHR=MIN(MXLCC,IBT(NODE)/8)
                  CATX(N,NCOL)=CHR(1:NCHR)
                  CALL USRTPL(LUN,1,1)
                  GOTO 100
               ENDIF
            ENDIF
         ENDDO
      ELSE

!        The message is not compressed. Locate the beginning of the data (Section 4) in the message.

         CALL GETLENS(MBAY(:,LUN),3,LEN0,LEN1,LEN2,LEN3,L4,L5)
         MBYTE = LEN0 + LEN1 + LEN2 + LEN3 + 4
         NSUBS = 1

!        Find the most recently written subset in the message.

         DO WHILE(NSUBS.LT.NSUB(LUN))
            IBIT = MBYTE*8
            CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
            MBYTE = MBYTE + NBYT
            NSUBS = NSUBS + 1
         ENDDO

         IF(NSUBS.NE.NSUB(LUN)) THEN
            IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: WRITLC - COULDN''T WRITE VALUE FOR ' // CTAG // ' INTO SUBSET, BECAUSE NO SUBSET WAS OPEN FOR WRITING'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
            GOTO 100
         ENDIF

!        Locate and write the long character string within this subset.

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
      ENDIF

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: WRITLC - COULDN''T WRITE VALUE FOR ' // CTAG // ' INTO SUBSET, BECAUSE IT WASN''T FOUND IN THE SUBSET' &
       // ' DEFINITION'
      CALL ERRWRT(ERRSTR)
      ERRSTR = '(' // CTAG // ' MAY NOT BE IN THE BUFR TABLE(?))'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4' &
       // ',")")') STR,NTG
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') CTAG,TYP(NODE)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THE MOST RECENTLY WRITTEN SUBSET NO. (",I3,") IN MSG .NE. THE STORED VALUE FOR THE NO.' &
       // ' OF SUBSETS (",I3,") IN MSG")') NSUBS,NSUB(LUN)
      CALL BORT(BORT_STR)

end
