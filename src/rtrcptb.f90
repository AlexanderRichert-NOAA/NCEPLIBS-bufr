!> @file
!> @brief Read the tank receipt time from Section 1 of a BUFR message.

!> This subroutine reads the tank receipt time (if one exists) from
!> Section 1 of a BUFR message.  It is similar to subroutine rtrcpt(),
!> except that it operates on a BUFR message passed in via a memory
!> array, whereas rtrcpt() operates on the BUFR message that was read
!> into internal arrays via the most recent call to any of the other
!> [message-reading subroutines](@ref hierarchy) for a specified
!> Fortran logical unit.
!>
!> @author J. Ator
!> @date 2013-10-07
!>
!> @param[in]  MBAY -- integer(*): BUFR message
!> @param[out] IYR  -- integer: Tank receipt year
!> @param[out] IMO  -- integer: Tank receipt month
!> @param[out] IDY  -- integer: Tank receipt day
!> @param[out] IHR  -- integer: Tank receipt hour
!> @param[out] IMI  -- integer: Tank receipt minute
!> @param[out] IRET -- integer: return code
!>                     - 0 = normal return
!>                     - -1 = no tank receipt time exists within MBAY
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2013-10-07 | J. Ator | Original author; adapted from rtrcpt() |
!> | 2022-02-01 | J. Ator  | Convert to Fortran90 |
!>

subroutine rtrcptb(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)

      use function_iupbs01
      use function_iupb

      integer :: MBAY(:)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IRET = -1

!     Check whether the message contains a tank receipt time.

      IF(IUPBS01(MBAY,'BEN').EQ.4) THEN
	IS1BYT = 23
      ELSE
	IS1BYT = 19
      ENDIF
      IF( (IS1BYT+5) .GT. IUPBS01(MBAY,'LEN1') ) RETURN

!     Unpack the tank receipt time.

!     Note that IS1BYT is a starting byte number relative to the beginning of Section 1, so we still need to account for
!     Section 0 when specifying the actual byte numbers to unpack within the overall message.

      IMGBYT = IS1BYT + IUPBS01(MBAY,'LEN0')

      IYR = IUPB(MBAY,IMGBYT,16)
      IMO = IUPB(MBAY,IMGBYT+2,8)
      IDY = IUPB(MBAY,IMGBYT+3,8)
      IHR = IUPB(MBAY,IMGBYT+4,8)
      IMI = IUPB(MBAY,IMGBYT+5,8)

      IRET = 0

      RETURN

end
