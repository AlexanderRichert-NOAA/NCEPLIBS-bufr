!> @file
!> @brief Read data descriptors from Section 3 of a BUFR message.

!> This subroutine returns the sequence of data descriptors contained within Section 3 of a BUFR message.
!>
!> @author J. Ator
!> @date 2003-11-04
!>
!> @param[in]  MBAY   -- integer(*): BUFR message
!> @param[in] LCDS3   -- integer: Dimensioned size of CDS3; used by the subroutine to ensure that it doesn't overflow the CDS3 array
!> @param[out] CDS3   -- character*6(*): Data descriptor sequence within Section 3 of MBAY
!> @param[out] NDS3   -- integer: Number of data descriptors in CDS3
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of MBAY.
!> - This subroutine does not recursively resolve any Table D descriptors from within Section 3; rather, what is returned in CDS3
!>   is the exact list of data descriptors as it appears within Section 3 of MBAY.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2003-11-04 | J. Ator | Original author |
!> | 2004-08-18 | J. Ator | Removed IFIRST check, since wrdlen() now keeps track of whether it has been called |
!> | 2005-11-29 | J. Ator | Use getlens() |
!> | 2009-03-23 | J. Ator | Added LCDS3 argument and check |
!> | 2022-02-01 | J. Ator | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_upds3

    private
    public upds3

    interface upds3
        module procedure upds3_4_d, upds3_8
    end interface

    contains

    subroutine upds3_4_d( mbay, lcds3, cds3, nds3 )
!       used when call arguments to upds3 are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:), lcds3
        character(len=6), intent(out) :: cds3(:)
        integer(kind=4), intent(out) :: nds3

        integer :: my_mbay(size(mbay)), my_lcds3, my_nds3

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))
        my_lcds3 = lcds3

        call upds3_body( my_mbay, my_lcds3, cds3, my_nds3  )

        nds3 = my_nds3

    end subroutine upds3_4_d

    subroutine upds3_8( mbay, lcds3, cds3, nds3 )
!       used when call arguments to upds3 are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:), lcds3
        character(len=6), intent(out) :: cds3(:)
        integer(kind=8), intent(out) :: nds3

        integer :: my_mbay(size(mbay)), my_lcds3, my_nds3

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))
        my_lcds3 = lcds3

        call upds3_body( my_mbay, my_lcds3, cds3, my_nds3  )

        nds3 = my_nds3

    end subroutine upds3_8

    subroutine upds3_body(MBAY,LCDS3,CDS3,NDS3)

        use subroutine_getlens
        use function_iupb

        integer, intent(in) :: MBAY(:)

        CHARACTER*6 CDS3(*), ADN30

!       Call subroutine WRDLEN to initialize some important information about the local machine, just in case subroutine OPENBF
!       hasn't been called yet.

        CALL WRDLEN

!       Skip to the beginning of Section 3.

        CALL GETLENS(MBAY,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
        IPT = LEN0 + LEN1 + LEN2

!       Unpack the Section 3 descriptors.

        NDS3 = 0
        DO JJ = 8,(LEN3-1),2
           NDS3 = NDS3 + 1
           IF(NDS3.GT.LCDS3) GOTO 900
           CDS3(NDS3) = ADN30(IUPB(MBAY,IPT+JJ,16),6)
        ENDDO

        RETURN
900     CALL BORT('BUFRLIB: UPDS3 - OVERFLOW OF OUTPUT DESCRIPTOR ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')

    end subroutine upds3_body

end module
