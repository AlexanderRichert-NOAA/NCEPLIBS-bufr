!> @file
!> @author WOOLLEN @date 1994-01-06
      
!> THIS SUBROUTINE COPIES A SUBSET FROM ONE MESSAGE BUFFER
!>   (ARRAY MBAY IN MODULE BITBUF) TO ANOTHER AND/OR RESETS THE
!>   POINTERS.  IF THE SUBSET WILL NOT FIT INTO THE OUTPUT MESSAGE, OR
!>   IF THE SUBSET BYTE COUNT EXCEEDS 65530 (SUFFICIENTLY CLOSE TO THE
!>   16-BIT BYTE COUNTER UPPER LIMIT OF 65535), THEN THAT MESSAGE IS
!>   FLUSHED TO LUNIT AND A NEW ONE IS CREATED IN ORDER TO HOLD THE
!>   COPIED SUBSET.  ANY SUBSET WITH BYTE COUNT > 65530 WILL BE WRITTEN
!>   INTO ITS OWN ONE-SUBSET MESSAGE.  IF THE SUBSET TO BE COPIED IS
!>   LARGER THAN THE MAXIMUM MESSAGE LENGTH, THEN A CALL IS ISSUED TO
!>   BUFR ARCHIVE LIBRARY SUBROUTINE BORT.
!>
!> PROGRAM HISTORY LOG:
!> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
!> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!>                           ROUTINE "BORT"
!> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!>                           BUFR FILES UNDER THE MPI)
!> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!>                           10,000 TO 20,000 BYTES
!> 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
!> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!>                           INTERDEPENDENCIES
!> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
!>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
!>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
!>                           TERMINATES ABNORMALLY
!> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!>                           20,000 TO 50,000 BYTES
!> 2009-03-23  J. ATOR    -- USE MSGFULL
!> 2014-10-27  J. WOOLLEN -- ACCOUNT FOR SUBSETS WITH BYTE COUNT > 65530
!>                           (THESE MUST BE WRITTEN INTO THEIR OWN
!>                           ONE-SUBSET MESSAGE)
!> 2014-10-27  D. KEYSER  -- FOR CASE ABOVE, DO NOT WRITE "CURRENT"
!>                           MESSAGE IF IT CONTAINS ZERO SUBSETS
!> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
!> 2015-09-24  D. STOKES  -- FIX MISSING DECLARATION OF COMMON /QUIET/
!>
!> USAGE:    CALL CPYUPD (LUNIT, LIN, LUN, IBYT)
!>   INPUT ARGUMENT LIST:
!>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!>     LIN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!>                FOR INPUT MESSAGE LOCATION
!>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!>                FOR OUTPUT MESSAGE LOCATION
!>     IBYT     - INTEGER: NUMBER OF BYTES OCCUPIED BY THIS SUBSET
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        BORT     ERRWRT   IUPB     MSGFULL
!>                               MSGINI   MSGWRT   MVB      PKB
!>    THIS ROUTINE IS CALLED BY: COPYSB
!>                               Normally not called by any application
!>                               programs.
!>

subroutine cpyupd(LUNIT,LIN,LUN,IBYT)

      USE MODA_MSGCWD
      USE MODA_BITBUF

      use subroutine_msgwrt
      use function_iupb

      COMMON /MSGPTR/ NBY0,NBY1,NBY2,NBY3,NBY4,NBY5

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR, ERRSTR

      LOGICAL MSGFULL

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

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

      IF(MSGFULL(MBYT(LUN),IBYT,MAXBYT)) GOTO 900

!  Transfer subset from one message to the other

!     Note that we want to append the data for this subset to the end of Section 4, but the value in MBYT(LUN) already includes the
!     length of Section 5 (i.e. 4 bytes).  Therefore, we need to begin writing at the point 3 bytes prior to the byte currently
!     pointed to by MBYT(LUN).

      CALL MVB(MBAY(1,LIN),MBYT(LIN)+1,MBAY(1,LUN),MBYT(LUN)-3,IBYT)

!  Update the subset and byte counters

      MBYT(LUN)   = MBYT(LUN)   + IBYT
      NSUB(LUN)   = NSUB(LUN)   + 1

      LBIT = (NBY0+NBY1+NBY2+4)*8
      CALL PKB(NSUB(LUN),16,MBAY(1,LUN),LBIT)

      LBYT = NBY0+NBY1+NBY2+NBY3
      NBYT = IUPB(MBAY(:,LUN),LBYT+1,24)
      LBIT = LBYT*8
      CALL PKB(NBYT+IBYT,24,MBAY(1,LUN),LBIT)

!  If the subset byte count is > 65530, then give it its own one-subset message (cannot have any other subsets in this message
!  because their beginning would be beyond the upper limit of 65535 in the 16-bit byte counter, meaning they could not be located!)

      IF(IBYT.GT.65530) THEN
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I7,A)') 'BUFRLIB: CPYUPD - SUBSET HAS BYTE COUNT = ',IBYT,' > UPPER LIMIT OF 65535'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>WILL BE COPIED INTO ITS OWN MESSAGE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         CALL MSGWRT(LUNIT,MBAY(:,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

!  EXITS
!  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CPYUPD - THE LENGTH OF THIS SUBSET EXCEEDS THE MAXIMUM MESSAGE LENGTH (",I6,")")') MAXBYT
      CALL BORT(BORT_STR)

end
