!> @file
!> @brief Copy a BUFR message.

!> This subroutine copies a BUFR message from internal arrays in memory to a specified Fortran logical unit.
!>
!> <p>This subroutine is similar to subroutine copymg(), except that
!> it copies a BUFR message from internal arrays in memory to a
!> specified Fortran logical unit, whereas copymg() copies a BUFR
!> message from one Fortran logical unit to another.
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> @param[in] LUNOT   -- integer: Fortran logical unit number for target BUFR file
!>
!> <p>One or more files of BUFR messages should have already been
!> read into internal arrays in memory via one or more previous
!> calls to subroutine ufbmem(), and a BUFR message should already
!> be in scope for processing from these arrays via a previous call
!> to subroutine rdmemm() or readmm().
!>
!> <p>Logical unit LUNOT should have already been opened for output
!> operations via a previous call to subroutine openbf(), but there
!> should not be any BUFR message already open for output within the
!> internal arrays for LUNOT via a previous call to one of the BUFRLIB
!> [message-writing subroutines](@ref hierarchy).
!>
!> <p>The [DX BUFR Table information](@ref dfbftab) associated with
!> the internal arrays in memory and with logical unit LUNOT must
!> contain identical definitions for the type of BUFR message to be
!> copied from the former to the latter.
!>
!> @remarks
!> - This subroutine uses subroutine msgwrt() to write to LUNOT; therefore, it can be used to transform a copy of the original
!> BUFR message from memory with any or all of the updates described in the documentation for subroutine msgwrt().
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI) |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2001-08-15 | D. Keyser  | Increased MAXMEM from 8 Mb to 16 Mb |
!> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2004-11-15 | D. Keyser  | Increased MAXMEM from 16 Mb to 50 Mb |
!> | 2005-11-29 | J. Ator    | Use iupbs01() |
!> | 2009-06-26 | J. Ator    | Use iok2cpy() |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Convert to Fortran90 |
!>

subroutine cpymem(LUNOT)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_MSGMEM
      USE MODA_TABLES

      use function_iupbs01
      use subroutine_msgwrt

      CHARACTER*8  SUBSET

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  Check the file statuses

      CALL STATUS(MUNIT,LIN,IL,IM)
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
900   CALL BORT('BUFRLIB: CPYMEM - LOGICAL UNIT NO. ASSOC. WITH INPUT BUFR MESSAGES IN INTERNAL MEMORY IS CLOSED,' // &
       ' IT MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: CPYMEM - LOGICAL UNIT NO. ASSOC. WITH INPUT BUFR MESSAGES IN INTERNAL MEMORY OPEN FOR OUTPUT,' // &
       ' MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: CPYMEM - A MESSAGE MUST BE OPEN IN INPUT BUFR MESSAGES IN INTERNAL MEMORY, NONE ARE')
903   CALL BORT('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: CPYMEM - ALL MESSAGES MUST BE CLOSED IN OUTPUT BUFR FILE, A MESSAGE IS OPEN')
906   CALL BORT('BUFRLIB: CPYMEM - INPUT BUFR MESSAGES IN INTERNAL MEMORY AND OUTPUT BUFR FILE MUST HAVE SAME INTERNAL ' // &
       ' TABLES (DIFFERENT HERE)')

end
