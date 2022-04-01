!> @file
!> @author ATOR @date 2005-11-29
	
!> GIVEN AN INTEGER ARRAY CONTAINING SECTION ZERO FROM A
!>   BUFR MESSAGE, THIS FUNCTION DETERMINES A COUNT OF MACHINE WORDS
!>   (I.E. INTEGER ARRAY MEMBERS) THAT WILL HOLD THE ENTIRE MESSAGE.
!>   NOTE THAT THIS COUNT MAY BE GREATER THAN THE MINIMUM NUMBER
!>   OF WORDS REQUIRED TO HOLD THE MESSAGE.
!>
!> PROGRAM HISTORY LOG:
!> 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
!>
!> USAGE:    NMWRD (MBAY)
!>   INPUT ARGUMENT LIST:
!>     MBAY     - INTEGER: *-WORD ARRAY CONTAINING SECTION ZERO
!>                FROM A BUFR MESSAGE
!>
!>   OUTPUT ARGUMENT LIST:
!>     NMWRD    - INTEGER: BUFR MESSAGE LENGTH (IN MACHINE WORDS)
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        IUPBS01
!>    THIS ROUTINE IS CALLED BY: CNVED4   CPDXMM   LMSG     MSGWRT
!>                               PADMSG   UFBMEM   UFBMEX
!>                               Also called by application programs.
!>

module function_nmwrd

    private
    public nmwrd

    interface nmwrd
        module procedure nmwrd_4_d, nmwrd_8
    end interface

    contains

    function nmwrd_4_d( mbay )
!       used when call arguments to nmwrd are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:)
        integer(kind=4) :: nmwrd_4_d

        integer :: my_mbay(size(mbay))

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        nmwrd_4_d = nmwrd_body( my_mbay )

    end function nmwrd_4_d

    function nmwrd_8( mbay )
!       used when call arguments to nmwrd are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:)
        integer(kind=8) :: nmwrd_8

        integer :: my_mbay(size(mbay))

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))

        nmwrd_8 = nmwrd_body( my_mbay )

    end function nmwrd_8

    function nmwrd_body(MBAY)

        use function_iupbs01
	
	COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

        integer, intent(in) :: MBAY(:)

	LENM = IUPBS01(MBAY,'LENM')
	IF(LENM.EQ.0) THEN
	    NMWRD_BODY = 0
	ELSE
	    NMWRD_BODY = ((LENM/8)+1)*(8/NBYTW)
	ENDIF

	RETURN
    end function nmwrd_body

end module
