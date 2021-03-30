C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS FUNCTION LOOKS FOR A SPECIFIED NODE WITHIN THE PORTION
C>   OF THE CURRENT SUBSET BUFFER BOUNDED BY THE INDICES INV1 AND INV2.
C>   IT IS SIMILAR TO BUFR ARCHIVE LIBRARY FUNCTION INVTAG, EXCEPT THAT
C>   INVTAG SEARCHES BASED ON THE MNEMONIC CORRESPONDING TO THE NODE.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C>                           WRF; ADDED DOCUMENTATION (INCLUDING
C>                           HISTORY); OUTPUTS MORE  COMPLETE DIAGNOSTIC
C>                           INFO WHEN UNUSUAL THINGS HAPPEN
C> 2009-03-31  J. WOOLLEN -- ADDED DOCUMENTATION
C> 2009-04-21  J. ATOR    -- USE ERRWRT
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    INVWIN (NODE, LUN, INV1, INV2)
C>   INPUT ARGUMENT LIST:
C>     NODE     - INTEGER: JUMP/LINK TABLE INDEX TO LOOK FOR
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>     INV1     - INTEGER: STARTING INDEX OF THE PORTION OF THE SUBSET
C>                BUFFER IN WHICH TO LOOK
C>     INV2     - INTEGER: ENDING INDEX OF THE PORTION OF THE SUBSET
C>                BUFFER IN WHICH TO LOOK
C>
C>   OUTPUT ARGUMENT LIST:
C>     INVWIN   - INTEGER: LOCATION INDEX OF NODE WITHIN SPECIFIED
C>                PORTION OF SUBSET BUFFER
C>                  0 = NOT FOUND
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        ERRWRT
C>    THIS ROUTINE IS CALLED BY: DRSTPL   GETWIN   NEVN     TRYBUMP
C>                               UFBGET   UFBRW    UFBSEQ
C>                               Normally not called by any application
C>                               programs.
C>
      FUNCTION INVWIN(NODE,LUN,INV1,INV2)

      USE MODA_USRINT

      COMMON /QUIET/  IPRT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      INVWIN = 0
      IF(NODE.EQ.0) GOTO 200

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

10    DO INVWIN=INV1,INV2
      IF(INV(INVWIN,LUN).EQ.NODE) GOTO 100
      ENDDO

      INVWIN = 0

 200  IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: INVWIN - RETURNING WITH A VALUE OF 0')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
