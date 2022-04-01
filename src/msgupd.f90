!> @file
!> @author WOOLLEN @date 1994-01-06
      
!> THIS SUBROUTINE PACKS UP THE CURRENT SUBSET WITHIN MEMORY
!>  (ARRAY IBAY IN MODULE BITBUF) AND THEN TRIES TO ADD IT TO
!>  THE BUFR MESSAGE THAT IS CURRENTLY OPEN WITHIN MEMORY FOR LUNIT
!>  (ARRAY MBAY IN MODULE BITBUF).  IF THE SUBSET WILL NOT FIT
!>  INTO THE CURRENTLY OPEN MESSAGE, OR IF THE SUBSET BYTE COUNT EXCEEDS
!>  65530 (SUFFICIENTLY CLOSE TO THE 16-BIT BYTE COUNTER UPPER LIMIT OF
!>  65535), THEN THAT MESSAGE IS FLUSHED TO LUNIT AND A NEW ONE IS
!>  CREATED IN ORDER TO HOLD THE CURRENT SUBSET.  ANY SUBSET WITH BYTE
!>  COUNT > 65530 WILL BE WRITTEN INTO ITS OWN ONE-SUBSET MESSAGE.
!>  IF THE CURRENT SUBSET IS LARGER THAN THE MAXIMUM MESSAGE LENGTH,
!>  THEN THE SUBSET IS DISCARDED AND A DIAGNOSTIC IS PRINTED.
!>
!> PROGRAM HISTORY LOG:
!> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
!> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!>                           ROUTINE "BORT"
!> 1998-12-14  J. WOOLLEN -- NO LONGER CALLS BORT IF A SUBSET IS LARGER
!>                           THAN A MESSAGE, JUST DISCARDS THE SUBSET
!> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!>                           BUFR FILES UNDER THE MPI)
!> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!>                           10,000 TO 20,000 BYTES
!> 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
!> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!>                           INTERDEPENDENCIES
!> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!>                           DOCUMENTATION
!> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!>                           20,000 TO 50,000 BYTES
!> 2009-03-23  J. ATOR    -- USE MSGFULL AND ERRWRT
!> 2014-10-20  J. WOOLLEN -- ACCOUNT FOR SUBSETS WITH BYTE COUNT > 65530
!>                           (THESE MUST BE WRITTEN INTO THEIR OWN
!>                           ONE-SUBSET MESSAGE)
!> 2014-10-20  D. KEYSER  -- FOR CASE ABOVE, DO NOT WRITE "CURRENT"
!>                           MESSAGE IF IT CONTAINS ZERO SUBSETS
!> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
!> 2016-03-21  D. STOKES  -- CALL USRTPL FOR OVERLARGE SUBSETS
!>
!> USAGE:    CALL MSGUPD (LUNIT, LUN)
!>   INPUT ARGUMENT LIST:
!>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!>                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        ERRWRT   IUPB     MSGFULL  MSGINI
!>                               MSGWRT   MVB      PAD      PKB
!>                               USRTPL   WRITLC
!>    THIS ROUTINE IS CALLED BY: WRITSA   WRITSB
!>                               Normally not called by any application
!>                               programs.
!>

subroutine msgupd(LUNIT,LUN)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_H4WLC

      use subroutine_msgwrt
      use function_iupb

      COMMON /MSGPTR/ NBY0,NBY1,NBY2,NBY3,NBY4,NBY5
      COMMON /QUIET / IPRT

      LOGICAL MSGFULL

      CHARACTER*128 ERRSTR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  Pad the subset buffer

      CALL PAD(IBAY,IBIT,IBYT,8)

!  Check whether the new subset should be written into the currently open message

      IF(MSGFULL(MBYT(LUN),IBYT,MAXBYT) &
            .OR. &
        ((IBYT.GT.65530).AND.(NSUB(LUN).GT.0))) THEN
!       NO it should not, either because:
!        1) it doesn't fit,
!                -- OR --
!        2) it has byte count > 65530 (sufficiently close to the upper limit for the 16 bit byte counter placed at the beginning
!           of each subset), AND the current message has at least one subset in it,
!       SO write the current message out and create a new one to hold the current subset
         CALL MSGWRT(LUNIT,MBAY(:,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

      IF(MSGFULL(MBYT(LUN),IBYT,MAXBYT)) THEN
!       This is an overlarge subset that won't fit in any message given the current value of MAXBYT, so discard the subset
!       and exit gracefully.
        IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        WRITE ( UNIT=ERRSTR, FMT='(A,I7,A)') &
         'BUFRLIB: MSGUPD - SUBSET LONGER THAN ANY POSSIBLE MESSAGE {MAXIMUM MESSAGE LENGTH = ', MAXBYT, '}'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>OVERLARGE SUBSET DISCARDED FROM FILE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
        ENDIF
        GOTO 100
      ENDIF

!  Set a byte count and transfer the subset buffer into the message

      LBIT = 0
      CALL PKB(IBYT,16,IBAY,LBIT)

!     Note that we want to append the data for this subset to the end of Section 4, but the value in MBYT(LUN) already includes the
!     length of Section 5 (i.e. 4 bytes).  Therefore, we need to begin writing at the point 3 bytes prior to the byte currently
!     pointed to by MBYT(LUN).

      CALL MVB(IBAY,1,MBAY(1,LUN),MBYT(LUN)-3,IBYT)

!  Update the subset and byte counters

      MBYT(LUN)   = MBYT(LUN)   + IBYT
      NSUB(LUN)   = NSUB(LUN)   + 1

      LBIT = (NBY0+NBY1+NBY2+4)*8
      CALL PKB(NSUB(LUN),16,MBAY(1,LUN),LBIT)

      LBYT = NBY0+NBY1+NBY2+NBY3
      NBYT = IUPB(MBAY(:,LUN),LBYT+1,24)
      LBIT = LBYT*8
      CALL PKB(NBYT+IBYT,24,MBAY(1,LUN),LBIT)

!  If any long character strings are being held internally for storage into this subset, store them now.

      IF(NH4WLC.GT.0) THEN
        DO II = 1, NH4WLC
          CALL WRITLC(LUH4WLC(II),CHH4WLC(II),STH4WLC(II))
        ENDDO
        NH4WLC = 0
      ENDIF

!  If the subset byte count is > 65530, then give it its own one-subset message (cannot have any other subsets in this message
!  because their beginning would be beyond the upper limit of 65535 in the 16-bit byte counter, meaning they could not be located!)

      IF(IBYT.GT.65530) THEN
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I7,A)') 'BUFRLIB: MSGUPD - SUBSET HAS BYTE COUNT = ',IBYT,' > UPPER LIMIT OF 65535'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>WILL BE WRITTEN INTO ITS OWN MESSAGE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         CALL MSGWRT(LUNIT,MBAY(:,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

!  Reset the user arrays and exit normally

100   CALL USRTPL(LUN,1,1)

!  EXIT
!  ----

      RETURN

end
