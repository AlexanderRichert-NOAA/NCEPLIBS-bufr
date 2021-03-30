C> @file
C> @author WOOLLEN @date 1999-11-18
      
C> THIS SUBROUTINE SEARCHES FOR MNEMONIC NEMO WITHIN THE
C>   INTERNAL TABLE A ARRAYS HOLDING THE DICTIONARY TABLE (ARRAYS IN
C>   MODULE TABABD) AND, IF FOUND, RETURNS INFORMATION ABOUT
C>   THAT MNEMONIC FROM WITHIN THESE ARRAYS.  IT IS IDENTICAL TO BUFR
C>   ARCHIVE LIBRARY SUBROUTINE NEMTBA EXCEPT THAT, IF NEMO IS NOT
C>   FOUND, THIS SUBROUTINE RETURNS WITH INOD EQUAL TO ZERO, WHEREAS
C>   NEMTBA CALLS BUFR ARCHIVE LIBRARY SUBROUTINE BORT IN SUCH CASES.
C>
C> PROGRAM HISTORY LOG:
C> 1999-11-18  J. WOOLLEN -- ORIGINAL AUTHOR
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL NEMTBAX (LUN, NEMO, MTYP, MSBT, INOD)
C>   INPUT ARGUMENT LIST:
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>     NEMO     - CHARACTER*(*): TABLE A MNEMONIC TO SEARCH FOR
C>
C>   OUTPUT ARGUMENT LIST:
C>     MTYP     - INTEGER: MESSAGE TYPE CORRESPONDING TO NEMO
C>     MSBT     - INTEGER: MESSAGE SUBTYPE CORRESPONDING TO NEMO
C>     INOD     - INTEGER: POSITIONAL INDEX OF NEMO WITHIN INTERNAL
C>                         JUMP/LINK TABLE IF NEMO FOUND
C>                       0 = NEMO not found
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT
C>    THIS ROUTINE IS CALLED BY: CKTABA   IOK2CPY  NEMTBA   STNDRD
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE NEMTBAX(LUN,NEMO,MTYP,MSBT,INOD)

      USE MODA_TABABD

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      INOD = 0

C  LOOK FOR NEMO IN TABLE A
C  ------------------------

      DO I=1,NTBA(LUN)
      IF(TABA(I,LUN)(4:11).EQ.NEMO) THEN
         MTYP = IDNA(I,LUN,1)
         MSBT = IDNA(I,LUN,2)
         INOD = MTAB(I,LUN)
         IF(MTYP.LT.0 .OR. MTYP.GT.255) GOTO 900
         IF(MSBT.LT.0 .OR. MSBT.GT.255) GOTO 901
         GOTO 100
      ENDIF
      ENDDO

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NEMTBAX - INVALID MESSAGE TYPE (",I4'//
     . ',") RETURNED FOR MENMONIC ",A)') MTYP,NEMO
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NEMTBAX - INVALID MESSAGE SUBTYPE ("'//
     . ',I4,") RETURNED FOR MENMONIC ",A)') MSBT,NEMO
      CALL BORT(BORT_STR)
      END
