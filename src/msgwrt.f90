!> @file
!> @brief Finalize a BUFR message for output and write the message to a BUFR file.

!> This subroutine performs final checks and updates on a BUFR message before writing it to a specified Fortran logical unit.
!>
!> <p>These final checks and updates include:
!> - Standardizing the BUFR message, if requested via a previous call subroutine stdmsg()
!> - Converting the BUFR message from edition 3 to edition 4, if requested via a previous call to subroutine pkvs01()
!> - Storing any customized values into Section 0 or Section 1 of the  BUFR message, if requested via one or more previous calls
!> to subroutine pkvs01()
!> - Storing a tank receipt time into Section 1 of the BUFR message, if requested via a previous call to subroutine strcpt()
!> - For edition 3 BUFR messages, ensuring each section of the message contains an even number of bytes
!> - Storing '7777' into the last four bytes of the BUFR message, and storing the final message length in Section 0
!> - Appending zeroed-out bytes after the end of the BUFR message, up to the next machine word boundary
!> - Encapsulating the BUFR message with IEEE Fortran control words, if requested via a previous call to subroutine setblock()
!> - Storing a copy of the final message into internal arrays for possible later retrival via subroutine writsa()
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
!> @param[in] MESG  -- integer(*): BUFR message
!> @param[in] MGBYT -- integer: Size (in bytes) of BUFR message
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1997-07-29 | J. Woollen | Modified to update the current BUFR version written in Section 0 from 2 to 3 |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
!> | 1998-11-24 | J. Woollen | Modified to zero out the padding bytes written at the end of Section 4 |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2003-11-04 | J. Ator    | Don't write to LUNIT if opened by openbf() using IO = 'NUL' |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2005-11-29 | J. Ator    | Use getlens(), iupbs01(), padmsg(), pkbs1() and nmwrd(); added logic to call pkbs1() and/or cnved4() when necessary |
!> | 2009-03-23 | J. Ator    | Use idxmsg() and errwrt(); add call to atrcpt(); allow standardizing even if data is compressed; work on local copy of input message |
!> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; call new routine blocks() for file blocking and new C routine cwrbufr() to write BUFR message to disk file |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2019-05-09 | J. Ator    | Added dimensions for MSGLEN and MSGTXT |
!> | 2022-02-01 | J. Ator    | Convert to Fortran90 |
!>

module subroutine_msgwrt

    contains

    subroutine msgwrt(LUNIT,MESG,MGBYT)

      USE MODA_NULBFR
      USE MODA_BUFRMG
      USE MODA_MGWA
      USE MODA_MGWB
      USE MODA_S01CM

      use function_idxmsg
      use function_iupbs01
      use function_nmwrd
      use subroutine_getlens

      integer, intent(in) :: lunit, mesg(:), mgbyt

      COMMON /QUIET / IPRT
      COMMON /MSGSTD/ CSMF
      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT

      CHARACTER*128 ERRSTR

      CHARACTER*4 BUFR,SEVN
      CHARACTER*1 CSMF
      CHARACTER*1 CTRT
      DIMENSION   IEC0(2)

      DATA BUFR/'BUFR'/
      DATA SEVN/'7777'/

!     Make a local copy of the input message for use within this subroutine, since calls to any or all of the subroutines STNDRD,
!     CNVED4, PKBS1, ATRCPT, ETC. may end up modifying the message before it finally gets written out to LUNIT.

      MBYT = MGBYT

      IEC0(1) = MESG(1)
      IEC0(2) = MESG(2)
      IBIT = 32
      CALL PKB(MBYT,24,IEC0,IBIT)

      DO II = 1, NMWRD(IEC0)
        MGWA(II) = MESG(II)
      ENDDO

!     Overwrite any values within Section 0 or Section 1 that were requested via previous calls to subroutine PKVS01.
!     If a request was made to change the BUFR edition number to 4, then actually convert the message as well.

      IF(NS01V.GT.0) THEN
        DO I=1,NS01V
          IF(CMNEM(I).EQ.'BEN') THEN
            IF(IVMNEM(I).EQ.4) THEN

!             Install Section 0 byte count for use by subroutine CNVED4.

              IBIT = 32
              CALL PKB(MBYT,24,MGWA,IBIT)

              CALL CNVED4(MGWA,MXMSGLD4,MGWB)

!             Compute MBYT for the new edition 4 message.

              MBYT = IUPBS01(MGWB,'LENM')

!             Copy the MGWB array back into MGWA.

              DO II = 1, NMWRD(MGWB)
                MGWA(II) = MGWB(II)
              ENDDO
            ENDIF
          ELSE

!           Overwrite the requested value.

            CALL PKBS1(IVMNEM(I),MGWA,CMNEM(I))
          ENDIF
        ENDDO
      ENDIF

!     "Standardize" the message if requested via COMMON /MSGSTD/.  However, we don't want to do this if the message contains
!     BUFR table (DX) information, in which case it is already "standard".

      IF ( ( CSMF.EQ.'Y' ) .AND. ( IDXMSG(MGWA).NE.1 ) )  THEN

!       Install Section 0 byte count and Section 5 '7777' into the original message.  This is necessary because subroutine STNDRD
!       requires a complete and well-formed BUFR message as its input.

        IBIT = 32
        CALL PKB(MBYT,24,MGWA,IBIT)
        IBIT = (MBYT-4)*8
        CALL PKC(SEVN,4,MGWA,IBIT)

        CALL STNDRD(LUNIT,MGWA,MXMSGLD4,MGWB)

!       Compute MBYT for the new "standardized" message.

        MBYT = IUPBS01(MGWB,'LENM')

!       Copy the MGWB array back into MGWA.

        DO II = 1, NMWRD(MGWB)
          MGWA(II) = MGWB(II)
        ENDDO
      ENDIF

!     Append the tank receipt time to Section 1 if requested via  COMMON /TNKRCP/, unless the message contains BUFR table (DX)
!     information. 

      IF ( ( CTRT.EQ.'Y' ) .AND. ( IDXMSG(MGWA).NE.1 ) ) THEN

!       Install Section 0 byte count for use by subroutine ATRCPT.

        IBIT = 32
        CALL PKB(MBYT,24,MGWA,IBIT)

        CALL ATRCPT(MGWA,MXMSGLD4,MGWB)

!       Compute MBYT for the revised message.

        MBYT = IUPBS01(MGWB,'LENM')

!       Copy the MGWB array back into MGWA.

        DO II = 1, NMWRD(MGWB)
          MGWA(II) = MGWB(II)
        ENDDO
      ENDIF

!     Get the section lengths.

      CALL GETLENS(MGWA,4,LEN0,LEN1,LEN2,LEN3,LEN4,L5)

!     Depending on the edition number of the message, we need to ensure that each section within the message contains an
!     even number of bytes.

      IF(IUPBS01(MGWA,'BEN').LT.4) THEN
        IF(MOD(LEN1,2).NE.0) GOTO 901
        IF(MOD(LEN2,2).NE.0) GOTO 902
        IF(MOD(LEN3,2).NE.0) GOTO 903
        IF(MOD(LEN4,2).NE.0) THEN

!          Pad Section 4 with an additional byte that is zeroed out.

           IAD4 = LEN0+LEN1+LEN2+LEN3
           IAD5 = IAD4+LEN4
           IBIT = IAD4*8
           LEN4 = LEN4+1
           CALL PKB(LEN4,24,MGWA,IBIT)
           IBIT = IAD5*8
           CALL PKB(0,8,MGWA,IBIT)
           MBYT = MBYT+1
        ENDIF
      ENDIF

!  Write Section 0 byte count and Section 5

      IBIT = 0
      CALL PKC(BUFR, 4,MGWA,IBIT)
      CALL PKB(MBYT,24,MGWA,IBIT)

      KBIT = (MBYT-4)*8
      CALL PKC(SEVN, 4,MGWA,KBIT)

!  Zero out the extra bytes which will be written

!     i.e. since the BUFR message is stored within the integer array MGWA(*) (rather than within a character array), we need to
!     ensure that the "7777" is followed by zeroed-out bytes up to the boundary of the last machine word that will be written out

      CALL PADMSG(MGWA,MXMSGLD4,NPBYT)

!  Write the message plus padding to a word boundary if NULL(LUN) = 0

      MWRD = NMWRD(MGWA)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(NULL(LUN).EQ.0) THEN
         CALL BLOCKS(MGWA,MWRD)
         CALL CWRBUFR(LUN,MGWA,MWRD)
      ENDIF

      IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I4,A,I7)') 'BUFRLIB: MSGWRT: LUNIT =', LUNIT, ', BYTES =', MBYT+NPBYT
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

!  Save a memory copy of this message, unless it's a DX message

      IF(IDXMSG(MGWA).NE.1) THEN

!        Store a copy of this message within module BUFRMG, for possible later retrieval during a future call to subroutine WRITSA

         MSGLEN(LUN) = MWRD
         DO I=1,MSGLEN(LUN)
           MSGTXT(I,LUN) = MGWA(I)
         ENDDO
      ENDIF

!  EXITS
!  -----

      RETURN
901   CALL BORT ('BUFRLIB: MSGWRT - LENGTH OF SECTION 1 IS NOT A MULTIPLE OF 2')
902   CALL BORT ('BUFRLIB: MSGWRT - LENGTH OF SECTION 2 IS NOT A MULTIPLE OF 2')
903   CALL BORT ('BUFRLIB: MSGWRT - LENGTH OF SECTION 3 IS NOT A MULTIPLE OF 2')

    end subroutine msgwrt

end
