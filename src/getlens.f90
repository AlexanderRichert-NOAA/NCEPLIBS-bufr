!> @file
!> @brief Read the section lengths of a BUFR message.

!> This subroutine reads the lengths of all of the individual sections of a given BUFR message, up to a specified point
!> in the message.
!>
!> <p>This subroutine will work on any BUFR message encoded using BUFR edition 2, 3, or 4
!>
!> @author J. Ator
!> @date 2005-11-29
!>
!> <b>Usage:</b> call getlens( MBAY, LL, LEN0, LEN1, LEN2, LEN3, LEN4, LEN5 )
!>
!> @param[in]  MBAY -- integer(*): BUFR message
!> @param[in]   LL  -- integer: Number of last section for which the length is to be read.
!>                     In other words, setting LL = N means to read and return the lengths of Sections 0
!>                     through N (i.e. LEN0, LEN1,...,LENN).  Any section lengths that are not specified
!>                     to be read are returned with a default placeholder value of -1.
!> @param[out]  LEN0 -- integer: Length (in bytes) of Section 0
!> @param[out]  LEN1 -- integer: Length (in bytes) of Section 1
!> @param[out]  LEN2 -- integer: Length (in bytes) of Section 2
!> @param[out]  LEN3 -- integer: Length (in bytes) of Section 3
!> @param[out]  LEN4 -- integer: Length (in bytes) of Section 4
!> @param[out]  LEN5 -- integer: Length (in bytes) of Section 5
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of MBAY.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2005-11-29 | J. Ator | Original author |
!> | 2022-02-01 | J. Ator  | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_getlens

    private
    public getlens

    interface getlens
        module procedure getlens_4_d, getlens_8
    end interface

    contains

    subroutine getlens_4_d( mbay, ll, len0, len1, len2, len3, len4, len5 )
!       used when call arguments to getlens are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:), ll
        integer(kind=4), intent(out) :: len0, len1, len2, len3, len4, len5

        integer :: my_mbay(size(mbay)), my_ll, my_len0, my_len1, my_len2, my_len3, my_len4, my_len5

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))
        my_ll = ll

        call getlens_body( my_mbay, my_ll, my_len0, my_len1, my_len2, my_len3, my_len4, my_len5 )

        len0 = my_len0
        len1 = my_len1
        len2 = my_len2
        len3 = my_len3
        len4 = my_len4
        len5 = my_len5

    end subroutine getlens_4_d

    subroutine getlens_8( mbay, ll, len0, len1, len2, len3, len4, len5 )
!       used when call arguments to getlens are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:), ll
        integer(kind=8), intent(out) :: len0, len1, len2, len3, len4, len5

        integer :: my_mbay(size(mbay)), my_ll, my_len0, my_len1, my_len2, my_len3, my_len4, my_len5

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))
        my_ll = ll

        call getlens_body( my_mbay, my_ll, my_len0, my_len1, my_len2, my_len3, my_len4, my_len5 )

        len0 = my_len0
        len1 = my_len1
        len2 = my_len2
        len3 = my_len3
        len4 = my_len4
        len5 = my_len5

    end subroutine getlens_8

    subroutine getlens_body(MBAY,LL,LEN0,LEN1,LEN2,LEN3,LEN4,LEN5) 

        use function_iupbs01
        use function_iupb

        integer, intent(in) :: MBAY(:)

	LEN0 = -1
	LEN1 = -1
	LEN2 = -1
	LEN3 = -1
	LEN4 = -1
	LEN5 = -1

	IF(LL.LT.0) RETURN
	LEN0 = IUPBS01(MBAY,'LEN0') 

	IF(LL.LT.1) RETURN
	LEN1 = IUPBS01(MBAY,'LEN1') 

	IF(LL.LT.2) RETURN
	IAD2 = LEN0 + LEN1
	LEN2 = IUPB(MBAY,IAD2+1,24) * IUPBS01(MBAY,'ISC2')

	IF(LL.LT.3) RETURN
	IAD3 = IAD2 + LEN2
	LEN3 = IUPB(MBAY,IAD3+1,24)

	IF(LL.LT.4) RETURN
	IAD4 = IAD3 + LEN3
	LEN4 = IUPB(MBAY,IAD4+1,24)

	IF(LL.LT.5) RETURN
	LEN5 = 4

	RETURN

    end subroutine getlens_body

end module
