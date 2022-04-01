!> @file
!> @brief Standardize a BUFR message.

!> This subroutine performs the same function as subroutine stdmsg(), except that it operates on a BUFR message passed in via
!> a memory array and returns its output via a separate memory array, whereas stdmsg() operates on BUFR messages stored internally
!> within the software.
!> 
!> @author J. Ator
!> @date 2004-08-18
!>
!> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR file
!> @param[in] MSGIN   -- integer(*): BUFR message
!> @param[in] LMSGOT  -- integer: Dimensioned size (in integers) of MSGOT; used by the subroutine to ensure that
!>                       it doesn't overflow the MSGOT array
!> @param[out] MSGOT  -- integer(*): Standardized copy of MSGIN
!>
!> @remarks
!> - MSGIN and MSGOT must be separate arrays.
!> - Standardized messages are usually longer in length than their non-standard counterparts, so it's usually a good idea to allow
!> for extra space when allocating MSGOT within the application program.
!> 
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2004-08-18 | J. Ator | Original author |
!> | 2005-11-29 | J. Ator | Use getlens() and iupbs01(); ensure that byte 4 of Section 4 is zeroed out in MSGOT; check edition number of BUFR message before padding to an even byte count |
!> | 2009-03-23 | J. Ator | Use iupbs3() and nemtbax(); don't assume that compressed messages are already fully standardized within Section 3 |
!> | 2014-02-04 | J. Ator | Account for subsets with byte count > 65530 |
!> | 2020-07-16 | J. Ator | Fix bug in ISLEN computation when NSUB = 1 |
!>

subroutine stndrd(LUNIT,MSGIN,LMSGOT,MSGOT)

      USE MODV_MAXNC

      use function_iupbs01
      use function_iupbs3
      use function_iupb
      use subroutine_getlens

      DIMENSION ICD(MAXNC)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      integer, intent(in) :: msgin(:)
      integer, intent(out) :: msgot(:)

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
      CHARACTER*4   SEVN
      CHARACTER*1   TAB

      LOGICAL FOUND

!  LUNIT must point to an open BUFR file

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

!  Identify the section lengths and addresses in MSGIN

      CALL GETLENS(MSGIN,5,LEN0,LEN1,LEN2,LEN3,LEN4,LEN5)

      IAD3 = LEN0+LEN1+LEN2
      IAD4 = IAD3+LEN3

      LENN = LEN0+LEN1+LEN2+LEN3+LEN4+LEN5

      LENM = IUPBS01(MSGIN,'LENM')

      IF(LENN.NE.LENM) GOTO 901

      MBIT = (LENN-4)*8
      CALL UPC(SEVN,4,MSGIN,MBIT,.TRUE.)
      IF(SEVN.NE.'7777') GOTO 902

!  Copy Sections 0 through part of Section 3 into MSGOT

      MXBYTO = (LMSGOT*NBYTW) - 8

      LBYTO = IAD3+7
      IF(LBYTO.GT.MXBYTO) GOTO 905
      CALL MVB(MSGIN,1,MSGOT,1,LBYTO)

!  Rewrite new Section 3 in a "standard" form

!     Locate the top-level Table A descriptor

      FOUND = .FALSE.
      II = 10
      DO WHILE ((.NOT.FOUND).AND.(II.GE.8))
          ISUB = IUPB(MSGIN,IAD3+II,16)
          CALL NUMTAB(LUN,ISUB,SUBSET,TAB,ITAB)
          IF((ITAB.NE.0).AND.(TAB.EQ.'D')) THEN
              CALL NEMTBAX(LUN,SUBSET,MTYP,MSBT,INOD)
              IF(INOD.NE.0) FOUND = .TRUE.
          ENDIF
          II = II - 2
      ENDDO
      IF(.NOT.FOUND) GOTO 903

      IF (ISTDESC(ISUB).EQ.0) THEN

!         ISUB is a non-standard Table A descriptor and needs to be expanded into an equivalent standard sequence  

          CALL RESTD(LUN,ISUB,NCD,ICD)
      ELSE

!         ISUB is already a standard descriptor, so just copy it "as is" into the new Section 3 (i.e. no expansion is necessary!)

          NCD = 1
          ICD(NCD) = ISUB
      ENDIF

!     Use the edition number to determine the length of the new Section 3

      LEN3 = 7+(NCD*2)
      IBEN = IUPBS01(MSGIN,'BEN')
      IF(IBEN.LT.4) THEN
          LEN3 = LEN3+1
      ENDIF
      LBYTO = LBYTO + LEN3 - 7
      IF(LBYTO.GT.MXBYTO) GOTO 905

!     Store the descriptors into the new Section 3

      IBIT = (IAD3+7)*8
      DO N=1,NCD
          CALL PKB(ICD(N),16,MSGOT,IBIT)
      ENDDO

!     Depending on the edition number, pad out the new Section 3 with an
!     additional zeroed-out byte in order to ensure an even byte count

      IF(IBEN.LT.4) THEN
          CALL PKB(0,8,MSGOT,IBIT)
      ENDIF

!     Store the length of the new Section 3

      IBIT = IAD3*8
      CALL PKB(LEN3,24,MSGOT,IBIT)

!  Now the tricky part - new Section 4

      IF(IUPBS3(MSGIN,'ICMP').EQ.1) THEN

!         The data in Section 4 is compressed and is therefore already standardized, so copy it "as is" into the new Section 4

          IF((LBYTO+LEN4+4).GT.MXBYTO) GOTO 905

          CALL MVB(MSGIN,IAD4+1,MSGOT,LBYTO+1,LEN4)

          JBIT = (LBYTO+LEN4)*8

      ELSE

          NAD4 = IAD3+LEN3

          IBIT = (IAD4+4)*8
          JBIT = (NAD4+4)*8

          LBYTO = LBYTO + 4

!         Copy the subsets, minus the byte counters and bit pads, into the new Section 4

          NSUB = IUPBS3(MSGIN,'NSUB')

          DO 10 I=1,NSUB
              CALL UPB(LSUB,16,MSGIN,IBIT)
              IF(NSUB.GT.1) THEN

!                 Use the byte counter to copy this subset

                  ISLEN = LSUB-2
              ELSE

!                 This is the only subset in the message, and it could possibly be an overlarge (> 65530 bytes) subset, in
!                 which case we can't rely on the value stored in the byte counter.  Either way, we don't really need it.

                  ISLEN = IAD4+LEN4-(IBIT/8)
                  IF (MOD(LEN4,2).EQ.0) ISLEN = ISLEN - 1
              ENDIF
              DO L=1,ISLEN
                  CALL UPB(NVAL,8,MSGIN,IBIT)
                  LBYTO = LBYTO + 1
                  IF(LBYTO.GT.MXBYTO) GOTO 905
                  CALL PKB(NVAL,8,MSGOT,JBIT)
              ENDDO
              DO K=1,8
                  KBIT = IBIT-K-8
                  CALL UPB(KVAL,8,MSGIN,KBIT)
                  IF(KVAL.EQ.K) THEN
                     JBIT = JBIT-K-8
                     GOTO 10
                  ENDIF
              ENDDO
              GOTO 904
10        ENDDO

!         From this point on, we will need (at most) 6 more bytes of space within MSGOT in order to be able to store the entire
!         standardized message (i.e. we will need (at most) 2 more zeroed-out bytes in Section 4 plus the 4 bytes '7777' in
!         Section 5), so do a final MSGOT overflow check now.

          IF(LBYTO+6.GT.MXBYTO) GOTO 905

!         Pad the new Section 4 with zeroes up to the next whole byte boundary.

          DO WHILE(.NOT.(MOD(JBIT,8).EQ.0))
             CALL PKB(0,1,MSGOT,JBIT)
          ENDDO

!         Depending on the edition number, we may need to further pad the new Section 4 with an additional zeroed-out byte in
!         order to ensure that the padding is up to an even byte boundary.

          IF( (IBEN.LT.4) .AND. (MOD(JBIT/8,2).NE.0) ) THEN
             CALL PKB(0,8,MSGOT,JBIT)
          ENDIF

          IBIT = NAD4*8
          LEN4 = JBIT/8 - NAD4
          CALL PKB(LEN4,24,MSGOT,IBIT)
          CALL PKB(0,8,MSGOT,IBIT)
      ENDIF

!  Finish the new message with an updated Section 0 byte count

      IBIT = 32
      LENN = LEN0+LEN1+LEN2+LEN3+LEN4+LEN5
      CALL PKB(LENN,24,MSGOT,IBIT)

      CALL PKC('7777',4,MSGOT,JBIT)

!  EXITS
!  -----

      RETURN
900   CALL BORT('BUFRLIB: STNDRD - BUFR FILE IS CLOSED, IT MUST BE OPEN')
901   WRITE(BORT_STR,'("BUFRLIB: STNDRD - INPUT MESSAGE LENGTH FROM SECTION 0",I6," DOES NOT EQUAL SUM OF ALL INDIVIDUAL SECTION' &
       // ' LENGTHS (",I6,")")') LENM,LENN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: STNDRD - INPUT MESSAGE DOES NOT END WITH ""7777"" (ENDS WITH ",A)') SEVN
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: STNDRD - TABLE A SUBSET DESCRIPTOR NOT FOUND')
904   CALL BORT('BUFRLIB: STNDRD - BIT MISMATCH COPYING SECTION 4 FROM INPUT TO OUTPUT (STANDARD) MESSAGE')
905   CALL BORT('BUFRLIB: STNDRD - OVERFLOW OF OUTPUT (STANDARD) MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')

end
