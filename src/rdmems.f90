!> @file
!> @brief Read a specified data subset from a BUFR message.

!> This subroutine reads a specified data subset from the BUFR message that was most recently read via a call to
!> subroutine rdmemm() or readmm().
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> @param[in] ISUB   -- integer: Number of data subset to be read from BUFR message, counting from the beginning of the message
!> @param[out] IRET  -- integer: return code
!>                         - 0 = requested data subset was successfully read
!>                         - -1 = requested subset number could not be found in the message
!>
!> <p>Whenever this subroutine returns with IRET = 0, this indicates that a new BUFR data subset (i.e. report) was successfully
!> read into internal arrays within the BUFRLIB software, and from where it can now be easily manipulated or further parsed via
!> calls to any of the [values-reading subroutines](@ref hierarchy) using the Fortran logical unit number IUNIT that was returned
!> from the most recent call to subroutine ufbmem().
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
!> | 1998-10-27 | J. Woollen | Modified to correct problems caused by in-lining code with fpp directives |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2001-08-15 | D. Keyser  | Increased MAXMEM from 8 Mb to 16 Mb |
!> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2004-11-15 | D. Keyser  | Increased MAXMEM from 16 Mb to 50 Mb |
!> | 2009-04-21 | J. Ator    | Use errwrt() |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Convert to Fortran90 |
!>

subroutine rdmems(ISUB,IRET)

      USE MODA_MSGCWD
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_MSGMEM

      use function_iupb

      CHARACTER*128 BORT_STR,ERRSTR

      COMMON /QUIET / IPRT

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  Check the message request and file status

      CALL STATUS(MUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(NSUB(LUN).NE.0) GOTO 903

      IF(ISUB.GT.MSUB(LUN)) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
           WRITE ( UNIT=ERRSTR, FMT='(A,I5,A,I5,A)' ) &
            'BUFRLIB: RDMEMS - REQ. SUBSET #', ISUB, ' (= 1st INPUT ARG.) > # OF SUBSETS IN MEMORY MESSAGE (', MSUB(LUN), ')'
           CALL ERRWRT(ERRSTR)
           CALL ERRWRT('RETURN WITH IRET = -1')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IRET = -1
         GOTO 100
      ENDIF

      MBYM = MBYT(LUN)
      NBYT = 0

!  Position to subset number ISUB in memory message

      IF(MSGUNP(LUN).EQ.0) THEN
         NSUB(LUN) = ISUB-1
         DO I=1,ISUB-1
         MBYT(LUN) = MBYT(LUN) + IUPB(MBAY(:,LUN),MBYT(LUN)+1,16)
         ENDDO
      ELSEIF(MSGUNP(LUN).EQ.1) THEN
!  .... message with "standard" Section 3
         DO I=1,ISUB-1
         CALL READSB(MUNIT,IRET)
         ENDDO
      ELSEIF(MSGUNP(LUN).EQ.2) THEN
!  .... compressed message
         NSUB(LUN) = ISUB-1
      ENDIF

!  Now read subset number ISUB from memory message

      CALL READSB(MUNIT,IRET)
!  .... This should have already been accounted for with stmt. 902 or
!       IRET = -1 above
      IF(IRET.NE.0) GOTO 904

!  Reset subset pointer back to zero (beginning of message) and return

      MBYT(LUN) = MBYM
      NSUB(LUN) = 0

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: RDMEMS - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: RDMEMS - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: RDMEMS - A MEMORY MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: RDMEMS - UPON ENTRY, SUBSET POINTER IN MEMORY MESSAGE IS NOT AT BEGINNING (",I3," SUBSETS HAVE ' &
       // 'BEEN READ, SHOULD BE 0)")') NSUB(LUN)
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: RDMEMS - CALL TO ROUTINE READSB RETURNED WITH IRET = -1 (EITHER MEMORY MESSAGE NOT OPEN OR ALL ' &
       // 'SUBSETS IN MESSAGE READ')

end
