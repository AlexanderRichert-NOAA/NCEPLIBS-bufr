C> @file
C> @author ATOR @date 2014-02-05
      
      SUBROUTINE HOLD4WLC(LUNIT,CHR,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    HOLD4WLC
C   PRGMMR: ATOR            ORG: NP12       DATE: 2014-02-05
C
C ABSTRACT:  NORMALLY, A LONG CHARACTER STRING (I.E. LONGER THAN 8
C   BYTES) IS STORED IN AN UNCOMPRESSED BUFR SUBSET FOR OUTPUT VIA A
C   CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE WRITLC, AT A POINT AFTER THE
C   CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE WRITSB (OR WRITSA) HAS
C   ALREADY BEEN MADE FOR THE SUBSET IN QUESTION.  THIS WORKS FINE FOR
C   ALL CASES EXCEPT WHEN WRITSB (OR WRITSA) FLUSHES THE MESSAGE
C   CONTAINING THE SUBSET IN QUESTION TO THE BUFR OUTPUT STREAM DURING
C   THE SAME CALL TO WRITSB (OR WRITSA), SUCH AS WHEN A SUBSET HAS A
C   BYTE COUNT > 65530 BYTES.  WHEN THIS HAPPENS, THERE IS NO LONGER ANY
C   WAY FOR A SUBSEQUENT WRITLC CALL TO STORE A LONG CHARACTER STRING IN
C   THE SUBSET, BECAUSE THE SUBSET HAS ALREADY BEEN FLUSHED FROM
C   INTERNAL MEMORY TO THE OUTPUT STREAM.  THIS SUBROUTINE GETS AROUND
C   THAT PROBLEM, BY ALLOWING A LONG CHARACTER STRING TO BE SPECIFIED
C   AHEAD OF TIME (I.E. BEFORE CALLING WRITSB OR WRITSB), AND THE
C   CORRESPONDING VALUE WILL BE HELD AND STORED AUTOMATICALLY (VIA AN
C   INTERNAL CALL TO WRITLC) AT THE PROPER TIME DURING THE SUBSEQUENT
C   CALL TO WRITSB (OR WRITSA).  IF MULTIPLE LONG CHARACTER STRINGS NEED
C   TO BE STORED IN A SUBSET, THEN A SEPARATE CALL TO THIS SUBROUTINE
C   SHOULD BE MADE FOR EACH SUCH STRING.
C
C PROGRAM HISTORY LOG:
C 2014-02-05  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL HOLD4WLC(LUNIT,CHR,STR)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     CHR      - CHARACTER*(*): UNPACKED CHARACTER STRING (I.E.,
C                CHARACTER DATA ELEMENT GREATER THAN EIGHT BYTES)
C     STR      - CHARACTER*(*): MNEMONIC ASSOCIATED WITH STRING IN CHR
C
C REMARKS:
C    THIS ROUTINE CALLS:        ERRWRT   STRSUC
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_H4WLC

      INCLUDE 'bufrlib.prm'

      COMMON /QUIET/ IPRT

      CHARACTER*(*) CHR,STR

      CHARACTER*128 ERRSTR
      CHARACTER*14  MYSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL STRSUC( STR, MYSTR, LENS )
      IF ( LENS .EQ. -1 ) RETURN

      LENC = MIN( LEN( CHR ), 120 )

C     IF THIS SUBROUTINE HAS ALREADY BEEN CALLED WITH THIS MNEMONIC FOR
C     THIS PARTICULAR SUBSET, THEN OVERWRITE THE CORRESPONDING ENTRY IN
C     THE INTERNAL HOLDING AREA.

      IF ( NH4WLC .GT. 0 ) THEN
        DO I = 1, NH4WLC
          IF ( ( LUNIT .EQ. LUH4WLC(I) ) .AND.
     .         ( MYSTR(1:LENS) .EQ. STH4WLC(I)(1:LENS) ) ) THEN
            CHH4WLC(I) = ''
            CHH4WLC(I)(1:LENC) = CHR(1:LENC)
            RETURN
          ENDIF
        ENDDO
      ENDIF

C     OTHERWISE, USE THE NEXT AVAILABLE UNUSED ENTRY IN THE HOLDING AREA.

      IF ( NH4WLC .GE. MXH4WLC ) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I3)' )
     . 'BUFRLIB: HOLD4WLC - THE MAXIMUM NUMBER OF LONG CHARACTER ',
     . 'STRINGS THAT CAN BE HELD INTERNALLY IS ', MXH4WLC
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
         ENDIF
      ELSE
         NH4WLC = NH4WLC + 1
         LUH4WLC(NH4WLC) = LUNIT
         STH4WLC(NH4WLC) = ''
         STH4WLC(NH4WLC)(1:LENS) = MYSTR(1:LENS)
         CHH4WLC(NH4WLC) = ''
         CHH4WLC(NH4WLC)(1:LENC) = CHR(1:LENC)
      ENDIF

      RETURN
      END
