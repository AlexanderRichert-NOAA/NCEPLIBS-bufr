!> @file
!> @brief Convert a BUFR edition 3 message to BUFR edition 4.

!> This subroutine reads an input BUFR message encoded using BUFR
!> edition 3 and outputs an equivalent BUFR message encoded using
!> BUFR edition 4.
!>
!> <p>This subroutine performs the same function as subroutine pkvs01()
!> when the latter is called with S01MNEM = 'BEN' and IVAL = 4, except
!> that the latter subroutine operates on BUFR messages internally
!> within the software, whereas this subroutine operates on a single
!> BUFR message passed in via a memory array.
!>
!> @author J. Ator
!> @date 2005-11-29
!>
!> @param[in] MSGIN   -- integer(*): BUFR message
!> @param[in] LMSGOT  -- integer: Dimensioned size (in integers) of
!>                       MSGOT; used by the subroutine to ensure that
!>                       it doesn't overflow the MSGOT array
!> @param[out] MSGOT  -- integer(*): Copy of MSGIN encoded using
!>                       BUFR edition 4
!>
!> @remarks
!> - MSGIN and MSGOT must be separate arrays.
!> - BUFR edition 4 messages are usually longer in length than their
!> BUFR edition 3 counterparts, so it's usually a good idea to allow
!> for extra space when allocating MSGOT within the application program.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2005-11-29 | J. Ator | Original author |
!> | 2009-08-12 | J. Ator | Allow silent return (instead of bort() return) if MSGIN is already encoded using edition 4 |
!> | 2022-02-01 | J. Ator  | Convert to Fortran90 |
!>

subroutine cnved4(MSGIN,LMSGOT,MSGOT)

        use function_iupbs01
        use function_nmwrd
        use subroutine_getlens

        integer :: MSGIN(:), MSGOT(:)

	COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

	IF(IUPBS01(MSGIN,'BEN').EQ.4) THEN

!	  The input message is already encoded using edition 4, so just
!	  copy it from MSGIN to MSGOT and then return.

	  NMW = NMWRD(MSGIN)
	  IF(NMW.GT.LMSGOT) GOTO 900 
	  DO I = 1, NMW
	    MSGOT(I) = MSGIN(I)
	  ENDDO
	  RETURN
	ENDIF

!	Get some section lengths and addresses from the input message.

	CALL GETLENS(MSGIN,3,LEN0,LEN1,LEN2,LEN3,L4,L5)

	IAD2 = LEN0 + LEN1
	IAD4 = IAD2 + LEN2 + LEN3 

	LENM = IUPBS01(MSGIN,'LENM')

!	Check for overflow of the output array.  Note that the new
!	edition 4 message will be a total of 3 bytes longer than the
!	input message (i.e. 4 more bytes in Section 1, but 1 fewer
!	byte in Section 3).

	LENMOT = LENM + 3
	IF(LENMOT.GT.(LMSGOT*NBYTW)) GOTO 900 

	LEN1OT = LEN1 + 4
	LEN3OT = LEN3 - 1

!	Write Section 0 of the new message into the output array.

	CALL MVB ( MSGIN, 1, MSGOT, 1, 4 )
	IBIT = 32
	CALL PKB ( LENMOT, 24, MSGOT, IBIT )
	CALL PKB ( 4, 8, MSGOT, IBIT )

!	Write Section 1 of the new message into the output array.

	CALL PKB ( LEN1OT, 24, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'BMT'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'OGCE'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'GSES'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'USN'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'ISC2')*128, 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTYP'), 8, MSGOT, IBIT )

!	Set a default of 255 for the international subcategory.

	CALL PKB ( 255, 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MSBT'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTV'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MTVL'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'YEAR'), 16, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MNTH'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'DAYS'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'HOUR'), 8, MSGOT, IBIT )
	CALL PKB ( IUPBS01(MSGIN,'MINU'), 8, MSGOT, IBIT )

!	Set a default of 0 for the second.

	CALL PKB ( 0, 8, MSGOT, IBIT )

!	Copy Section 2 (if it exists) through the next-to-last byte of Section 3 from the input array to the output array.

	CALL MVB ( MSGIN, IAD2+1, MSGOT, (IBIT/8)+1, LEN2+LEN3-1 )

!	Store the length of the new Section 3.

	IBIT = ( LEN0 + LEN1OT + LEN2 ) * 8
	CALL PKB ( LEN3OT, 24, MSGOT, IBIT )
	
!	Copy Section 4 and Section 5 from the input array to the output array.

	IBIT = IBIT + ( LEN3OT * 8 ) - 24
	CALL MVB ( MSGIN, IAD4+1, MSGOT, (IBIT/8)+1, LENM-IAD4 )

	RETURN
900	CALL BORT('BUFRLIB: CNVED4 - OVERFLOW OF OUTPUT (EDITION 4) MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')

end
