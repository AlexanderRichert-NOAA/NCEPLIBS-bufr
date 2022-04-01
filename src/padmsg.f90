!> @file
!> @author ATOR @date 2005-11-29
	
!> THIS SUBROUTINE PADS A BUFR MESSAGE WITH ZEROED-OUT BYTES
!>  FROM THE END OF THE MESSAGE UP TO THE NEXT 8-BYTE BOUNDARY.
!>
!> PROGRAM HISTORY LOG:
!> 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
!>
!> USAGE:    CALL PADMSG (MESG, LMESG, NPBYT )
!>   INPUT ARGUMENT LIST:
!>     MESG     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
!>                MESSAGE 
!>     LMESG    - INTEGER: DIMENSIONED SIZE (IN INTEGER WORDS) OF MESG;
!>                USED BY THE SUBROUTINE TO ENSURE THAT IT DOES NOT
!>                OVERFLOW THE MESG ARRAY
!>
!>   OUTPUT ARGUMENT LIST:
!>     MESG     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
!>                MESSAGE WITH NPBYT ZEROED-OUT BYTES APPENDED TO THE END
!>     NPBYT    - INTEGER: NUMBER OF ZEROED-OUT BYTES APPENDED TO MESG
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        BORT     IUPBS01  NMWRD    PKB
!>    THIS ROUTINE IS CALLED BY: MSGWRT
!>                               Also called by application programs.
!>

subroutine padmsg(MESG,LMESG,NPBYT)

        use function_iupbs01
        use function_nmwrd

	COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

        integer :: MESG(:)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!	Make sure that the array is big enough to hold the additional
!	byte padding that will be appended to the end of the message.

	NMW = NMWRD(MESG)
	IF(NMW.GT.LMESG) GOTO 900

!	Pad from the end of the message up to the next 8-byte boundary.

	NMB = IUPBS01(MESG,'LENM')
	IBIT = NMB*8
	NPBYT = ( NMW * NBYTW ) - NMB
	DO I = 1, NPBYT
	    CALL PKB(0,8,MESG,IBIT)
	ENDDO

	RETURN
900     CALL BORT('BUFRLIB: PADMSG - CANNOT ADD PADDING TO MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')

end
