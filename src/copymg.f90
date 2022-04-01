!> @file
!> @brief Copy a BUFR message.

!> This subroutine copies a BUFR message from one Fortran logical unit to another.
!>
!> <p>This subroutine is similar to subroutine cpymem(), except that
!> it copies a BUFR message from one Fortran logical unit to another,
!> whereas cpymem() copies a BUFR message from internal arrays in
!> memory to a specified Fortran logical unit.
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> @param[in] LUNIN   -- integer: Fortran logical unit number for source BUFR file
!> @param[in] LUNOT   -- integer: Fortran logical unit number for target BUFR file
!>
!> <p>Logical unit LUNIN should have already been opened for input
!> operations via a previous call to subroutine openbf(), and a BUFR
!> message should have already been read into internal arrays for
!> LUNIN via a previous call to one of the
!> [message-reading subroutines](@ref hierarchy).
!>
!> <p>Logical unit LUNOT should have already been opened for output
!> operations via a previous call to subroutine openbf(), but there
!> should not be any BUFR message already open for output within the
!> internal arrays for LUNOT via a previous call to one of the BUFRLIB
!> [message-writing subroutines](@ref hierarchy).
!>
!> <p>The [DX BUFR Table information](@ref dfbftab) associated with
!> each of the logical units LUNIN and LUNOT must contain identical
!> definitions for the type of BUFR message to be copied from LUNIN
!> to LUNOT.
!>
!> @remarks
!> - This subroutine uses subroutine msgwrt() to write to LUNOT; therefore, it can be used to transform a copy of the original
!> BUFR message from LUNIN with any or all of the updates described in the documentation for subroutine msgwrt().
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI) |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2004-08-09 | J. Ator | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-11-29 | J. Ator | Use iupbs01() |
!> | 2009-06-26 | J. Ator | Use iok2cpy() |
!> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator | Convert to Fortran90 |
!>

subroutine copymg(LUNIN,LUNOT)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      use function_iupbs01
      use subroutine_msgwrt

      CHARACTER*8  SUBSET

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  Check the file statuses

      CALL STATUS(LUNIN,LIN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      CALL STATUS(LUNOT,LOT,IL,IM)
      IF(IL.EQ.0) GOTO 903
      IF(IL.LT.0) GOTO 904
      IF(IM.NE.0) GOTO 905

!  Make sure both files have the same tables

      SUBSET = TAG(INODE(LIN))
!  .... Given SUBSET, returns MTYP,MSBT,INOD
      CALL NEMTBA(LOT,SUBSET,MTYP,MSBT,INOD)
      IF(INODE(LIN).NE.INOD) THEN
        IF(IOK2CPY(LIN,LOT).NE.1) GOTO 906
      ENDIF

!  Everything okay, copy a message

      MBYM = IUPBS01(MBAY(:,LIN),'LENM')
      CALL MSGWRT(LUNOT,MBAY(:,LIN),MBYM)

!  Set the message control words for partition associated with LUNOT

      NMSG (LOT) = NMSG(LOT) + 1
      NSUB (LOT) = MSUB(LIN)
      MSUB (LOT) = MSUB(LIN)
      IDATE(LOT) = IDATE(LIN)
      INODE(LOT) = INOD

!  EXITS
!  -----

      RETURN
900   CALL BORT('BUFRLIB: COPYMG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: COPYMG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: COPYMG - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: COPYMG - ALL MESSAGES MUST BE CLOSED IN OUTPUT BUFR FILE, A MESSAGE IS OPEN')
906   CALL BORT('BUFRLIB: COPYMG - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')

end
