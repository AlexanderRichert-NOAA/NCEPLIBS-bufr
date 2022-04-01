!> @file
!> @brief Copy a BUFR data subset.

!> This subroutine copies a BUFR data subset from one Fortran logical unit to another.
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> @param[in] LUNIN   -- integer: Fortran logical unit number for source BUFR file
!> @param[in] LUNOT   -- integer: Fortran logical unit number for target BUFR file
!> @param[out] IRET   -- integer: return code
!>                       - 0 = normal return
!>                       - -1 = a BUFR data subset could not be read from the BUFR message in internal arrays for LUNIN
!>
!> <p>Logical unit LUNIN should have already been opened for input operations via a previous call to subroutine openbf(),
!> and a BUFR message should have already been read into internal arrays for LUNIN via a previous call to one of the
!> [message-reading subroutines](@ref hierarchy).
!>
!> <p>Logical unit LUNOT should have already been opened for output operations via a previous call to subroutine openbf(),
!> and a BUFR message should already be open for output within internal arrays via a previous call to one of the BUFRLIB
!> [message-writing subroutines](@ref hierarchy).
!>
!> <p>The compression status of the data subset (i.e. compressed or uncompressed) will be preserved when copying from
!> LUNIN to LUNOT.
!>
!> <p>If LUNOT < 0, then a data subset is read from the BUFR message in internal arrays for LUNIN but is not copied to the BUFR
!> message in internal arrays for LUNOT.  Otherwise, the [DX BUFR Table information](@ref dfbftab) associated with each of the
!> logical units LUNIN and LUNOT must contain identical definitions for the type of BUFR message containing the data
!> subset to be copied from LUNIN to LUNOT.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI) |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2002-05-14 | J. Woollen | Removed old Cray compiler directives |
!> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-09-16 | J. Woollen | Now writes out compressed subset/message if input subset/message is compressed |
!> | 2009-06-26 | J. Ator    | Use iok2cpy() |
!> | 2014-11-03 | J. Ator    | Handle oversized (>65530 bytes) subsets |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator  | Convert to Fortran90 |
!>

subroutine copysb(LUNIN,LUNOT,IRET)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      use subroutine_getlens

      CHARACTER*128 BORT_STR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IRET = 0

!  Check the file statuses

      CALL STATUS(LUNIN,LIN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      IF(LUNOT.GT.0) THEN
         CALL STATUS(LUNOT,LOT,IL,IM)
         IF(IL.EQ.0) GOTO 903
         IF(IL.LT.0) GOTO 904
         IF(IM.EQ.0) GOTO 905
         IF(INODE(LIN).NE.INODE(LOT)) THEN
           IF( (TAG(INODE(LIN)).NE.TAG(INODE(LOT))) .OR. (IOK2CPY(LIN,LOT).NE.1) ) GOTO 906
         ENDIF
      ENDIF

!  See if there is another subset in the message

      IF(NSUB(LIN).EQ.MSUB(LIN)) THEN
         IRET = -1
         GOTO 100
      ENDIF

!  Check compression status of input message, output message will match

      CALL MESGBC(-LUNIN,MEST,ICMP)

      IF(ICMP.EQ.1) THEN

!  -------------------------------------------------------
!  This branch is for compressed input/output messages
!  -------------------------------------------------------
!  Read in and uncompress subset, then copy it to compressed output msg

         CALL READSB(LUNIN,IRET)
         IF(LUNOT.GT.0) THEN
            CALL UFBCPY(LUNIN,LUNOT)
            CALL CMPMSG('Y')
            CALL WRITSB(LUNOT)
            CALL CMPMSG('N')
         ENDIF
         GOTO 100
      ELSE  IF(ICMP.EQ.0) THEN

!  -------------------------------------------------------
!  This branch is for uncompressed input/output messages
!  -------------------------------------------------------
!  Copy the subset to the output message and/or reset the pointers

         IBIT = (MBYT(LIN))*8
         CALL UPB(NBYT,16,MBAY(1,LIN),IBIT)
         IF (NBYT.GT.65530) THEN

!          This is an oversized subset, so we can't rely on the value
!          of NBYT as being the true size (in bytes) of the subset.

           IF ( (NSUB(LIN).EQ.0) .AND. (MSUB(LIN).EQ.1) )  THEN

!            But it's also the first and only subset in the message,
!            so we can determine its true size in a different way.

             CALL GETLENS(MBAY(:,LIN),4,LEN0,LEN1,LEN2,LEN3,LEN4,L5)
             NBYT = LEN4 - 4
           ELSE

!            We have no way to easily determine the true size of this
!            oversized subset.

             IRET = -1
             GOTO 100
           ENDIF
         ENDIF
         IF(LUNOT.GT.0) CALL CPYUPD(LUNOT,LIN,LOT,NBYT)
         MBYT(LIN) = MBYT(LIN) + NBYT
         NSUB(LIN) = NSUB(LIN) + 1
      ELSE
         GOTO 907
      ENDIF

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: COPYSB - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: COPYSB - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')
906   CALL BORT('BUFRLIB: COPYSB - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')
907   WRITE(BORT_STR,'("BUFRLIB: COPYSB - INVALID COMPRESSION INDICATOR (ICMP=",I3," RETURNED FROM BUFR ARCHIVE LIBRARY '// &
       'ROUTINE MESGBC")') ICMP
      CALL BORT(BORT_STR)

end
