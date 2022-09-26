C> @file
C> @brief Read a specified BUFR message from internal arrays.

C> This subroutine reads a specified BUFR message from internal
C> arrays in memory, so that it is now in scope for processing
C> via a subsequent call to subroutine rdmems().
C>
C> <p>BUFR messages should already be stored within internal
C> arrays in memory via one or more previous calls to
C> subroutine ufbmem().
C>
C> <p>This subroutine is similar to subroutine rdmemm(), except that
C> this subroutine increments the value of IMSG prior to returning to
C> the calling program, which in turn allows it to be easily called
C> within an iterative program loop.
C>
C> @author J. Woollen
C> @date 1999-11-18
C>
C> @param[in,out] IMSG -- integer: Message pointer within internal arrays
C>                        - On input, IMSG is the number of the BUFR
C>                          message to be read into scope for further
C>                          processing, counting from the beginning of
C>                          the internal arrays in memory
C>                        - On output, IMSG is incremented by one from
C>                          its input value
C> @param[out] SUBSET -- character*8: Table A mnemonic for type of BUFR
C>                       message that was read into scope
C>                       (see [DX BUFR Tables](@ref dfbftab) for
C>                       further information about Table A mnemonics)
C> @param[out] JDATE  -- integer: Date-time stored within Section 1 of
C>                       BUFR message that was read into scope,
C>                       in format of either YYMMDDHH or YYYYMMDDHH,
C>                       depending on the most
C>                       recent call to subroutine datelen()
C> @param[out] IRET   -- integer: return code
C>                          - 0 = requested message was
C>                                successfully read into scope
C>                          - -1 = requested message number could not
C>                                 be found in internal arrays
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1999-11-18 | J. Woollen | Original author |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2001-08-15 | D. Keyser  | Increased MAXMEM from 8 Mb to 16 Mb |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2004-11-15 | D. Keyser  | Increased MAXMEM from 16 Mb to 50 Mb |
C> | 2009-03-23 | J. Ator    | Rewrote to call rdmemm() |
C> | 2022-08-04 | J. Ator    | Added 8-byte wrapper |

      SUBROUTINE READMM(IMSG,SUBSET,JDATE,IRET)

      USE MODV_IM8B

      CHARACTER*8 SUBSET
      INTEGER*8 IMSG_8,JDATE_8,IRET_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         IMSG_8=IMSG
         CALL READMM_8(IMSG_8,SUBSET,JDATE_8,IRET_8)
         IMSG=IMSG_8
         JDATE=JDATE_8
         IRET=IRET_8

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL RDMEMM(IMSG,SUBSET,JDATE,IRET)

      IMSG = IMSG+1

      RETURN
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine readmm().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine readmm() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in,out] IMSG_8 -- integer*8: Message pointer within internal arrays
C> @param[out] SUBSET -- character*8: Table A mnemonic for type of BUFR
C>                       message that was read into scope
C> @param[out] JDATE_8 -- integer*8: Date-time stored within Section 1 of
C>                        BUFR message that was read into scope
C> @param[out] IRET_8  -- integer*8: return code
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Ator    | Original author      |

      SUBROUTINE READMM_8(IMSG_8,SUBSET,JDATE_8,IRET_8)

      INTEGER*8 IMSG_8,JDATE_8,IRET_8
      CHARACTER*8   SUBSET

      IMSG=IMSG_8
      CALL READMM(IMSG,SUBSET,JDATE,IRET)
      IMSG_8=IMSG
      JDATE_8=JDATE
      IRET_8=IRET

      RETURN
      END
