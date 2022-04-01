!> @file
!> @brief Read a data value from Section 3 of a BUFR message.

!> This function returns a specified value from within Section 3 of a BUFR message.
!>
!> @author J. Ator
!> @date 2009-03-23
!>
!> @param[in]  MBAY   -- integer(*): BUFR message
!> @param[in]  S3MNEM  -- character*(*): Value to be read from Section 3 of MBAY
!>                         - 'NSUB'  = Number of data subsets
!>                         - 'IOBS'  = Flag indicating whether the message contains observed data:
!>                                     - 0 = No
!>                                     - 1 = Yes
!>                         - 'ICMP'  = Flag indicating whether the message contains compressed data:
!>                                     - 0 = No
!>                                     - 1 = Yes
!> @returns iupbs3 -- integer: Value corresponding to S3MNEM
!>                      - -1 = S3MNEM was invalid 
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of MBAY.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2009-03-23 | J. Ator | Original author |
!> | 2022-02-01 | J. Ator | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module function_iupbs3

    private
    public iupbs3

    interface iupbs3
        module procedure iupbs3_4_d, iupbs3_8
    end interface

    contains

    function iupbs3_4_d( mbay, s3mnem )
!       used when call arguments to iupbs3 are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:)
        character(len=*), intent(in) :: s3mnem
        integer(kind=4) :: iupbs3_4_d

        integer :: my_mbay(size(mbay))

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        iupbs3_4_d = iupbs3_body( my_mbay, s3mnem )

    end function iupbs3_4_d

    function iupbs3_8( mbay, s3mnem )
!       used when call arguments to iupbs3 are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:)
        character(len=*), intent(in) :: s3mnem
        integer(kind=8) :: iupbs3_8

        integer :: my_mbay(size(mbay))

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        iupbs3_8 = iupbs3_body( my_mbay, s3mnem )

    end function iupbs3_8

    function iupbs3_body(MBAY,S3MNEM)

        use subroutine_getlens
        use function_iupb

	integer, intent(in) :: MBAY(:)

	CHARACTER*(*)	S3MNEM

!	Call subroutine WRDLEN to initialize some important information about the local machine, just in case subroutine OPENBF
!	hasn't been called yet.

	CALL WRDLEN

!	Skip to the beginning of Section 3.

	CALL GETLENS(MBAY,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
	IPT = LEN0 + LEN1 + LEN2

!	Unpack the requested value.

	IF(S3MNEM.EQ.'NSUB') THEN
	    IUPBS3_BODY = IUPB(MBAY,IPT+5,16)
	ELSE IF( (S3MNEM.EQ.'IOBS') .OR. (S3MNEM.EQ.'ICMP') ) THEN
	    IVAL = IUPB(MBAY,IPT+7,8)
	    IF(S3MNEM.EQ.'IOBS') THEN
		IMASK = 128
	    ELSE
		IMASK = 64
	    ENDIF
	    IUPBS3_BODY = MIN(1,IAND(IVAL,IMASK))
	ELSE
	    IUPBS3_BODY = -1
	ENDIF

	RETURN

    end function iupbs3_body

end module
