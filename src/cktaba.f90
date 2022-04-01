!> @file
!> @author WOOLLEN @date 2000-09-19
      
!> THIS SUBROUTINE PARSES THE TABLE A MNEMONIC AND THE DATE
!>   OUT OF SECTION 1 OF A BUFR MESSAGE PREVIOUSLY READ FROM UNIT LUNIT
!>   USING BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR EQUIVALENT (AND NOW
!>   STORED IN THE INTERNAL MESSAGE BUFFER, ARRAY MBAY IN MODULE
!>   BITBUF).  THE TABLE A MNEMONIC IS ASSOCIATED WITH THE BUFR
!>   MESSAGE TYPE/SUBTYPE IN SECTION 1.  IT ALSO FILLS IN THE MESSAGE
!>   CONTROL WORD PARTITION ARRAYS IN MODULE MSGCWD.
!>
!> PROGRAM HISTORY LOG:
!> 2000-09-19  J. WOOLLEN -- ORIGINAL AUTHOR - CONSOLIDATED MESSAGE
!>                           DECODING LOGIC THAT HAD BEEN REPLICATED IN
!>                           READMG, READFT, READERME, RDMEMM AND READIBM
!>                           (CKTABA IS NOW CALLED BY THESE CODES);
!>                           LOGIC ENHANCED HERE TO ALLOW COMPRESSED AND
!>                           STANDARD BUFR MESSAGES TO BE READ
!> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!>                           INTERDEPENDENCIES
!> 2003-11-04  D. KEYSER  -- MODIFIED TO NOT ABORT WHEN THE SECTION 1
!>                           MESSAGE SUBTYPE DOES NOT AGREE WITH THE
!>                           SECTION 1 MESSAGE SUBTYPE IN THE DICTIONARY
!>                           IF THE MESSAGE TYPE MNEMONIC IS NOT OF THE
!>                           FORM "NCtttsss", WHERE ttt IS THE BUFR TYPE
!>                           AND sss IS THE BUFR SUBTYPE (E.G., IN
!>                           "PREPBUFR" FILES); MODIFIED DATE
!>                           CALCULATIONS TO NO LONGER USE FLOATING
!>                           POINT ARITHMETIC SINCE THIS CAN LEAD TO
!>                           ROUND OFF ERROR AND AN IMPROPER RESULTING
!>                           DATE ON SOME MACHINES (E.G., NCEP IBM
!>                           FROST/SNOW), INCREASES PORTABILITY;
!>                           UNIFIED/PORTABLE FOR WRF; ADDED
!>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
!>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
!>                           TERMINATES ABNORMALLY OR UNUSUAL THINGS
!>                           HAPPEN; SUBSET DEFINED AS "        " IF
!>                           IRET RETURNED AS 11 (BEFORE WAS UNDEFINED)
!> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!>                           20,000 TO 50,000 BYTES
!> 2005-11-29  J. ATOR    -- USE IUPBS01, IGETDATE AND GETLENS
!> 2006-04-14  J. ATOR    -- ALLOW "FRtttsss" AND "FNtttsss" AS POSSIBLE
!>                           TABLE A MNEMONICS, WHERE ttt IS THE BUFR
!>                           TYPE AND sss IS THE BUFR SUBTYPE
!> 2009-03-23  J. ATOR    -- ADD LOGIC TO ALLOW SECTION 3 DECODING;
!>                           USE IUPBS3 AND ERRWRT
!> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
!>
!> USAGE:    CALL CKTABA (LUN, SUBSET, JDATE, IRET)
!>   INPUT ARGUMENT LIST:
!>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!>
!>   OUTPUT ARGUMENT LIST:
!>     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE BEING CHECKED:
!>                       "        " = IRET equal to 11 (see IRET below) and not using Section 3 decoding
!>     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR MESSAGE BEING CHECKED, IN FORMAT OF EITHER YYMMDDHH OR
!>                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE 
!>     IRET     - INTEGER: RETURN CODE:
!>                       0 = normal return
!>                      -1 = unrecognized Table A (message type) value
!>                      11 = this is a BUFR table (dictionary) message
!>
!> REMARKS:
!>    THIS ROUTINE CALLS:        BORT     DIGIT    ERRWRT   GETLENS
!>                               I4DY     IGETDATE IUPB     IUPBS01
!>                               IUPBS3   NEMTBAX  NUMTAB   OPENBT
!>                               RDUSDX
!>    THIS ROUTINE IS CALLED BY: RDMEMM   READERME READMG
!>                               Normally not called by any application
!>                               programs.
!>

subroutine cktaba(LUN,SUBSET,JDATE,IRET)

      USE MODA_MSGCWD
      USE MODA_SC3BFR
      USE MODA_UNPTYP
      USE MODA_BITBUF

      use function_iupbs01
      use function_iupbs3
      use function_iupb
      use function_igetdate
      use subroutine_getlens

      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   SUBSET
      CHARACTER*2   CPFX(3)
      CHARACTER*1   TAB
      LOGICAL       TRYBT, DIGIT

      DATA CPFX   / 'NC', 'FR', 'FN' /
      DATA NCPFX  / 3 /

      IRET = 0

      TRYBT = .TRUE.

      JDATE = IGETDATE(MBAY(:,LUN),IYR,IMO,IDY,IHR)

!  .... Message type
      MTYP = IUPBS01(MBAY(:,LUN),'MTYP')
!  .... Message subtype
      MSBT = IUPBS01(MBAY(:,LUN),'MSBT')

      IF(MTYP.EQ.11) THEN
!  .... This is a BUFR table (dictionary) message.
         IRET = 11
!  .... There's no need to proceed any further unless Section 3 is being used for decoding.
         IF(ISC3(LUN).EQ.0) THEN
            SUBSET = "        "
            GOTO 100
         ENDIF
      ENDIF

!  Parse Section 3

      CALL GETLENS(MBAY(:,LUN),3,LEN0,LEN1,LEN2,LEN3,L4,L5)

      IAD3 = LEN0+LEN1+LEN2

!  .... First descriptor (integer)
      KSUB = IUPB(MBAY(:,LUN),IAD3+8 ,16)
!  .... Second descriptor (integer)
      ISUB = IUPB(MBAY(:,LUN),IAD3+10,16)

!  Locate Section 4

      IAD4 = IAD3+LEN3

!  Now, try to get "SUBSET" (mnemonic associated with Table A) from message

!  First check whether Section 3 is being used for decoding

      IF(ISC3(LUN).NE.0) THEN
	SUBSET = TAMNEM(LUN)
!  .... is SUBSET from Table A?
	CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
	IF(INOD.GT.0) THEN
!  ....	  yes it is
	  MBYT(LUN) = 8*(IAD4+4)
	  MSGUNP(LUN) = 1
	  GOTO 10
	ENDIF
      ENDIF

!  If ISUB from Section 3 defines Table A then MSGUNP=0

!  .... get SUBSET from ISUB
5     CALL NUMTAB(LUN,ISUB,SUBSET,TAB,ITAB)
!  .... is SUBSET from Table A?
      CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
      IF(INOD.GT.0) THEN
!  .... yes it is
         MBYT(LUN) = (IAD4+4)
         MSGUNP(LUN) = 0
         GOTO 10
      ENDIF

!  If KSUB from Section 3 defines Table A then MSGUNP=1 (standard)

!  .... get SUBSET from KSUB
      CALL NUMTAB(LUN,KSUB,SUBSET,TAB,ITAB)
!  .... is SUBSET from Table A?
      CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
      IF(INOD.GT.0) THEN
!  .... yes it is
         MBYT(LUN) = 8*(IAD4+4)
         MSGUNP(LUN) = 1
         GOTO 10
      ENDIF

!  Okay, still no "SUBSET", so lets make it "NCtttsss" (where ttt=MTYP and sss=MSBT) and see if it defines Table A.
!  If not, then also try "FRtttsss" AND "FNtttsss".

      II=1
      DO WHILE(II.LE.NCPFX)
         WRITE(SUBSET,'(A2,2I3.3)') CPFX(II),MTYP,MSBT
!  ....    is SUBSET from Table A?
         CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
         IF(INOD.GT.0) THEN
!  ....     yes it is
            IF(KSUB.EQ.IBCT) THEN
               MBYT(LUN) = (IAD4+4)
               MSGUNP(LUN) = 0
            ELSE
               MBYT(LUN) = 8*(IAD4+4)
               MSGUNP(LUN) = 1
            ENDIF
            GOTO 10
         ENDIF
         II=II+1
      ENDDO

!  Now we have a generated "SUBSET", but it still does not define Table A - make one last desperate attempt - see if an external
!  user-supplied BUFR dictionary table in character format is defined in OPENBT (only possible if application program has an
!  in-line OPENBT overriding the one in the library)

      IF(TRYBT) THEN
         TRYBT = .FALSE.
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      ERRSTR = 'BUFRLIB: CKTABA - LAST RESORT, CHECK FOR EXTERNAL BUFR TABLE VIA CALL TO IN-LINE OPENBT'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         CALL OPENBT(LUNDX,MTYP)
         IF(LUNDX.GT.0) THEN
!  .... Good news, there is a unit (LUNDX) connected to a table file, so store the table internally
            CALL RDUSDX(LUNDX,LUN)
            GOTO 5
         ENDIF
      ENDIF

!  If all attempts to define Table A fail, then give up

      IF(IPRT.GE.0)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: CKTABA - UNRECOGNIZED TABLE A MESSAGE TYPE (' // SUBSET // ') - RETURN WITH IRET = -1'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      IRET = -1
      GOTO 100

!  Check the validity of the MTYP/MSBT and for compression (MSGUNP=2)

10    IF(ISC3(LUN).EQ.0) THEN
        IF(MTYP.NE.MTY1) GOTO 900
        IF(MSBT.NE.MSB1.AND.DIGIT(SUBSET(3:8))) GOTO 901
      ENDIF
      IF(IUPBS3(MBAY(:,LUN),'ICMP').GT.0) MSGUNP(LUN) = 2

!  Set the other required parameters in message control word partition

!  .... Date for this message
      IDATE(LUN) = I4DY(JDATE)
!  .... Positional index of Table A mnem.
      INODE(LUN) = INOD
!  .... Number of subsets in this message
      MSUB(LUN) = IUPBS3(MBAY(:,LUN),'NSUB')
!  .... Number of subsets read so far from this message
      NSUB(LUN) = 0

      IF(IRET.NE.11) THEN
!   .... Number of non-dictionary messages read so far from this file
         NMSG(LUN) = NMSG(LUN)+1
      ENDIF

!  EXITS
!  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CKTABA - MESSAGE TYPE MISMATCH (SUBSET=",A8,", MTYP=",I3,", MTY1=",I3)') SUBSET,MTYP,MTY1
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: CKTABA - MESSAGE SUBTYPE MISMATCH (SUBSET=",A8,", MSBT=",I3,", MSB1=",I3)') SUBSET,MSBT,MSB1
      CALL BORT(BORT_STR)

end
