C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE READS AND EXAMINES SECTION 1 OF MESSAGES
C>  IN A BUFR FILE IN SEQUENCE UNTIL IT FINDS THE FIRST MESSAGE THAT
C>  IS NOT A BUFR TABLE (DICTIONARY) MESSAGE.  IT THEN RETURNS THE
C>  MESSAGE TYPE FOR THIS FIRST NON-DICTIONARY MESSAGE.  THE BUFR FILE
C>  SHOULD NOT BE OPEN VIA BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF PRIOR
C>  TO CALLING THIS SUBROUTINE; HOWEVER, THE BUFR FILE MUST BE CONNECTED
C>  TO UNIT LUNIT.  THIS SUBROUTINE IS IDENTICAL TO BUFR ARCHIVE LIBRARY
C>  SUBROUTINE MESGBC EXCEPT THAT MESGBC RETURNS THE MESSAGE TYPE FOR
C>  THE FIRST NON-DICTIONARY MESSAGE THAT ACTUALLY CONTAINS REPORT DATA
C>  (WHEREAS MESGBF WOULD RETURN THE REPORT TYPE OF A DUMMY MESSAGE
C>  CONTAINING THE CENTER TIME FOR DUMP FILES), AND MESGBC ALSO
C>  INDICATES WHETHER OR NOT THE FIRST REPORT DATA MESSAGE IS BUFR
C>  COMPRESSED.  MESGBC ALSO HAS AN OPTION TO OPERATE ON THE CURRENT
C>  MESSAGE STORED IN MEMORY, WHICH IS SOMETHING THAT MESGBF CANNOT DO.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY)
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2005-11-29  J. ATOR    -- USE IUPBS01 AND RDMSGW
C> 2009-03-23  J. ATOR    -- USE IDXMSG
C> 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C>                           USE NEW OPENBF TYPE 'INX' TO OPEN AND CLOSE
C>                           THE C FILE WITHOUT CLOSING THE FORTRAN FILE
C> 2013-01-25  J. WOOLLEN -- ALWAYS CALL CLOSBF BEFORE EXITING
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL MESGBF (LUNIT, MESGTYP)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>
C>   OUTPUT ARGUMENT LIST:
C>     MESGTYP  - INTEGER: BUFR MESSAGE TYPE FOR FIRST NON-DICTIONARY
C>                MESSAGE
C>                      -1 = no messages read or error
C>                      11 = if only BUFR table messages in BUFR file
C>
C>   INPUT FILES:
C>     UNIT "LUNIT" - BUFR FILE
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        CLOSBF   IDXMSG   IUPBS01  OPENBF
C>                               RDMSGW
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      SUBROUTINE MESGBF(LUNIT,MESGTYP)

      USE MODA_MGWA

      use subroutine_closbf
      use subroutine_openbf

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      MESGTYP = -1

C  SINCE OPENBF HAS NOT YET BEEN CALLED, CALL IT 
C  ---------------------------------------------

      CALL OPENBF(LUNIT,'INX',LUNIT)

C  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND
C  -----------------------------------------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.EQ.0) THEN
         MESGTYP = IUPBS01(MGWA,'MTYP')
         IF(IDXMSG(MGWA).EQ.1) GOTO 1
      ENDIF

C  CLOSE THE FILE
C  --------------
  
      CALL CLOSBF(LUNIT)

C  EXIT
C  ----

100   RETURN
      END
