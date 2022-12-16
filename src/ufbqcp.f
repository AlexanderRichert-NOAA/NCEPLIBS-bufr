C> @file
C> @brief Get the Table D mnemonic associated with an event program
C> code from an NCEP prepbufr file

C> Given an event program code, which is equivalent to the Y value
C> of a category 63 (i.e. X=63) Table D descriptor from an NCEP
C> prepbufr file, this subroutine returns the corresponding
C> mnemonic.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                     NCEP prepbufr file
C> @param[in] IQCP -- integer: Y value of a category 63 (i.e. X=63)
C>                    Table D descriptor
C> @param[out] NEMO  -- character*(*): Mnemonic associated with IQCP
C>
C> @remarks
C> - Logical unit LUNIT should have already been opened via a previous
C> call to subroutine openbf().
C> - This subroutine is the logical inverse of subroutine ufbqcd().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE UFBQCP(LUNIT,IQCP,NEMO)

      USE MODV_IM8B

      CHARACTER*(*) NEMO
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(IQCP,MY_IQCP,1)
         CALL UFBQCP(MY_LUNIT,MY_IQCP,NEMO)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      IDN = IFXY('363000')+IQCP
c  .... get NEMO from IDN
      CALL NUMTAB(LUN,IDN,NEMO,TAB,IRET)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBQCP - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
      END
