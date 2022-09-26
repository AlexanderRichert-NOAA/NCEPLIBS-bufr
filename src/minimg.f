C> @file
C> @brief Write a minutes value into Section 1 of a BUFR message.

C> This subroutine writes a minutes value into Section 1 of the BUFR
C> message that was most recently opened for writing via a call to
C> one of the [message-writing subroutines](@ref hierarchy) for a
C> specified Fortran logical unit.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                              BUFR file
C> @param[in] MINI  -- integer: Minutes value
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
C> | 2002-05-14 | J. Woollen | Changed from an entry point to increase portability to other platforms |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-11-29 | J. Ator    | Use pkbs1() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE MINIMG(LUNIT,MINI)

      USE MODA_BITBUF
      USE MODV_IM8B

      INTEGER*8 LUNIT_8,MINI_8

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         MINI_8=MINI
         CALL MINIMG_8(LUNIT_8,MINI_8)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      CALL PKBS1(MINI,MBAY(1,LUN),'MINU')

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: MINIMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: MINIMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: MINIMG - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine minimg().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine minimg() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C> @param[in] MINI_8  -- integer*8: Minutes value
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE MINIMG_8(LUNIT_8,MINI_8)

      INTEGER*8 LUNIT_8,MINI_8

      LUNIT=LUNIT_8
      MINI=MINI_8
      CALL MINIMG(LUNIT,MINI)

      RETURN
      END
