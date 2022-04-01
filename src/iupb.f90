!> @file
!> @author WOOLLEN @date 1994-01-06
      
!> THIS FUNCTION UNPACKS AND RETURNS A BINARY INTEGER WORD
!>   CONTAINED WITHIN NBIT BITS OF A BUFR MESSAGE PACKED INTO THE
!>   INTEGER ARRAY MBAY, STARTING WITH THE FIRST BIT OF BYTE NBYT.
!>
!> PROGRAM HISTORY LOG:
!> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
!> 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
!> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!>                           INTERDEPENDENCIES
!> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!>                           DOCUMENTATION
!>
!> USAGE:    IUPB (MBAY, NBYT, NBIT)
!>   INPUT ARGUMENT LIST:
!>     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
!>                MESSAGE
!>     NBYT     - INTEGER: BYTE WITHIN MBAY AT WHOSE FIRST BIT TO BEGIN
!>                UNPACKING
!>     NBIT     - INTEGER: NUMBER OF BITS WITHIN MBAY TO BE UNPACKED
!>
!>   OUTPUT ARGUMENT LIST:
!>     IUPB     - INTEGER: UNPACKED INTEGER WORD
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        UPB
!>    THIS ROUTINE IS CALLED BY: CKTABA   CPYUPD   GETLENS  IUPBS01
!>                               IUPBS3   MSGUPD   MSGWRT   RDMEMS
!>                               RTRCPTB  STBFDX   STNDRD   STRCPT
!>                               UPDS3    WRDXTB   WRITLC
!>                               Normally not called by any application
!>                               programs.
!>

module function_iupb

    private
    public iupb

    interface iupb
        module procedure iupb_4_d, iupb_8
    end interface

    contains

    function iupb_4_d( mbay, nbyt, nbit )
!       used when call arguments to iupb are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: mbay(:), nbyt, nbit
        integer(kind=4) :: iupb_4_d

        integer :: my_mbay(size(mbay)), my_nbyt, my_nbit

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))
        my_nbyt = nbyt
        my_nbit = nbit

        iupb_4_d = iupb_body( my_mbay, my_nbyt, my_nbit )

    end function iupb_4_d

    function iupb_8( mbay, nbyt, nbit )
!       used when call arguments to iupb are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: mbay(:), nbyt, nbit
        integer(kind=8) :: iupb_8

        integer :: my_mbay(size(mbay)), my_nbyt, my_nbit

        my_mbay(1:size(mbay)) = mbay(1:size(mbay))
        my_nbyt = nbyt
        my_nbit = nbit

        iupb_8 = iupb_body( my_mbay, my_nbyt, my_nbit )

    end function iupb_8

    function iupb_body(MBAY,NBYT,NBIT)

      integer, intent(in) :: mbay(:)

      MBIT = (NBYT-1)*8
      CALL UPB(IRET,NBIT,MBAY,MBIT)
      IUPB_BODY = IRET
      RETURN

    end function iupb_body

end module
