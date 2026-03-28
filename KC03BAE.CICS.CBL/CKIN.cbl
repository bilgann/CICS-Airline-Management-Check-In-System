       IDENTIFICATION DIVISION.
       PROGRAM-ID. CKIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY CKINMAP.

       01 WS-RESP                  PIC S9(8) COMP.
       01 WS-ERROR-MESSAGE         PIC X(35)
           VALUE 'CKIN: UNABLE TO DISPLAY SCREEN'.
       01 WS-FORMATTED-DATE        PIC X(11).
       01 WS-TIME-FORMATTED        PIC X(5).
       01 WS-LAST-NAME-RAW         PIC X(16).
       01 WS-PNR-RAW               PIC X(7).
       01 WS-LAST-NAME-IN          PIC X(16).
       01 WS-PNR-IN                PIC X(6).

       01 WS-COMMAREA.
           05 WS-CA-STATE          PIC X VALUE SPACE.
           05 WS-CA-NAME           PIC X(18) VALUE SPACES.
           05 WS-CA-PASSPORT       PIC X(16) VALUE SPACES.
           05 WS-CA-ORIG           PIC X(3) VALUE SPACES.
           05 WS-CA-DEST           PIC X(3) VALUE SPACES.
           05 WS-CA-DEPDATE        PIC X(8) VALUE SPACES.
           05 WS-CA-RETDATE        PIC X(8) VALUE SPACES.
           05 WS-CA-TRIPTYPE       PIC X VALUE SPACE.
           05 WS-CA-OUT-FLT        PIC X(6) VALUE SPACES.
           05 WS-CA-OUT-DEP        PIC X(4) VALUE SPACES.
           05 WS-CA-OUT-ARR        PIC X(4) VALUE SPACES.
           05 WS-CA-RET-FLT        PIC X(6) VALUE SPACES.
           05 WS-CA-RET-DEP        PIC X(4) VALUE SPACES.
           05 WS-CA-RET-ARR        PIC X(4) VALUE SPACES.
           05 WS-CA-PNR            PIC X(6) VALUE SPACES.

       01 WS-MONTH-TABLE.
           05 FILLER               PIC X(3) VALUE 'JAN'.
           05 FILLER               PIC X(3) VALUE 'FEB'.
           05 FILLER               PIC X(3) VALUE 'MAR'.
           05 FILLER               PIC X(3) VALUE 'APR'.
           05 FILLER               PIC X(3) VALUE 'MAY'.
           05 FILLER               PIC X(3) VALUE 'JUN'.
           05 FILLER               PIC X(3) VALUE 'JUL'.
           05 FILLER               PIC X(3) VALUE 'AUG'.
           05 FILLER               PIC X(3) VALUE 'SEP'.
           05 FILLER               PIC X(3) VALUE 'OCT'.
           05 FILLER               PIC X(3) VALUE 'NOV'.
           05 FILLER               PIC X(3) VALUE 'DEC'.
       01 WS-MONTH-NAMES REDEFINES WS-MONTH-TABLE.
           05 WS-MONTH-NAME OCCURS 12 TIMES PIC X(3).

       01 WS-MONTH-NUM             PIC 99.

      * PNR VSAM record layout (91 bytes)
       01 WS-PNR-REC.
           05 PR-PNR               PIC X(6).
           05 PR-LAST-NAME         PIC X(16).
           05 PR-NAME              PIC X(18).
           05 PR-ORIG              PIC X(3).
           05 PR-DEST              PIC X(3).
           05 PR-DEPDATE           PIC X(8).
           05 PR-RETDATE           PIC X(8).
           05 PR-TRIPTYPE          PIC X.
           05 PR-OUT-FLT           PIC X(6).
           05 PR-OUT-DEP           PIC X(4).
           05 PR-OUT-ARR           PIC X(4).
           05 PR-RET-FLT           PIC X(6).
           05 PR-RET-DEP           PIC X(4).
           05 PR-RET-ARR           PIC X(4).

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           05 CA-STATE             PIC X.
           05 CA-NAME              PIC X(18).
           05 CA-PASSPORT          PIC X(16).
           05 CA-ORIG              PIC X(3).
           05 CA-DEST              PIC X(3).
           05 CA-DEPDATE           PIC X(8).
           05 CA-RETDATE           PIC X(8).
           05 CA-TRIPTYPE          PIC X.
           05 CA-OUT-FLT           PIC X(6).
           05 CA-OUT-DEP           PIC X(4).
           05 CA-OUT-ARR           PIC X(4).
           05 CA-RET-FLT           PIC X(6).
           05 CA-RET-DEP           PIC X(4).
           05 CA-RET-ARR           PIC X(4).
           05 CA-PNR               PIC X(6).

       PROCEDURE DIVISION USING DFHCOMMAREA.

      * =======================================================
      *                   MAIN LOGIC
      * =======================================================
       MAIN-SECTION.
           MOVE SPACES TO WS-COMMAREA

           IF EIBCALEN > 0
               MOVE DFHCOMMAREA TO WS-COMMAREA
           END-IF

           IF EIBAID = DFHPF3
               EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF1
               EXEC CICS SEND CONTROL ERASE END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF

           EXEC CICS
               RECEIVE MAP('CKINMAP') MAPSET('CKINMAP')
               INTO(CKINMAPI)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               IF WS-CA-STATE = 'K'
                  AND WS-CA-OUT-FLT NOT = SPACES
                   MOVE 'D' TO WS-CA-STATE
                   EXEC CICS
                       XCTL PROGRAM('CKIN2')
                            COMMAREA(WS-COMMAREA)
                            LENGTH(92)
                   END-EXEC
                   EXEC CICS RETURN END-EXEC
               ELSE
                   PERFORM SEND-INITIAL-SCREEN
                   PERFORM RETURN-TO-CKIN
               END-IF
           END-IF

           IF EIBAID = DFHENTER
               PERFORM NORMALIZE-INPUT
               IF WS-CA-STATE = 'K'
                  AND WS-PNR-IN = WS-CA-PNR
                  AND WS-CA-OUT-FLT NOT = SPACES
                   MOVE 'D' TO WS-CA-STATE
                   EXEC CICS
                       XCTL PROGRAM('CKIN2')
                            COMMAREA(WS-COMMAREA)
                            LENGTH(92)
                   END-EXEC
                   EXEC CICS RETURN END-EXEC
               END-IF

               PERFORM PROCESS-CHECKIN
           ELSE
               PERFORM SEND-INITIAL-SCREEN
           END-IF

           PERFORM RETURN-TO-CKIN.

      * =======================================================
      *                SEND INITIAL SCREEN
      * =======================================================
       SEND-INITIAL-SCREEN.
           MOVE LOW-VALUES TO CKINMAPO
           MOVE SPACE TO WS-CA-STATE
           PERFORM HIDE-FLIGHT-SECTIONS
           EXEC CICS
               SEND MAP('CKINMAP') MAPSET('CKINMAP')
               FROM(CKINMAPO)
               ERASE
           END-EXEC.

      * =======================================================
      *                  PROCESS CHECK-IN
      * =======================================================
       PROCESS-CHECKIN.
           MOVE LOW-VALUES TO CKINMAPO

           MOVE WS-LAST-NAME-IN TO LASTVO
           MOVE WS-PNR-IN TO PNRVO

           IF WS-LAST-NAME-IN(1:1) = SPACE
              OR WS-PNR-IN(1:1) = SPACE
              OR WS-PNR-IN(6:1) = SPACE
               MOVE SPACE TO WS-CA-STATE
               MOVE 'ENTER LAST NAME AND 6-CHARACTER PNR' TO MSGO
               PERFORM HIDE-FLIGHT-SECTIONS
               PERFORM SEND-CHECKIN-SCREEN
               EXIT PARAGRAPH
           END-IF

           EXEC CICS
               READ FILE('PNRDATA')
                    INTO(WS-PNR-REC)
                    RIDFLD(WS-PNR-IN)
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE SPACE TO WS-CA-STATE
               MOVE 'BOOKING NOT FOUND FOR THAT PNR' TO MSGO
               PERFORM HIDE-FLIGHT-SECTIONS
               PERFORM SEND-CHECKIN-SCREEN
               EXIT PARAGRAPH
           END-IF

           IF PR-LAST-NAME NOT = WS-LAST-NAME-IN
               MOVE SPACE TO WS-CA-STATE
               MOVE 'LAST NAME DOES NOT MATCH THIS PNR' TO MSGO
               PERFORM HIDE-FLIGHT-SECTIONS
               PERFORM SEND-CHECKIN-SCREEN
               EXIT PARAGRAPH
           END-IF

           PERFORM STORE-CHECKIN-COMMAREA
           PERFORM POPULATE-FLIGHT-DISPLAY
           PERFORM SEND-CHECKIN-SCREEN.

      * =======================================================
      *                NORMALIZE INPUT
      * =======================================================
       NORMALIZE-INPUT.
           MOVE SPACES TO WS-LAST-NAME-IN
           MOVE SPACES TO WS-PNR-IN

           MOVE LASTVI TO WS-LAST-NAME-RAW
           MOVE PNRVI TO WS-PNR-RAW

           INSPECT WS-LAST-NAME-RAW REPLACING ALL LOW-VALUES BY SPACE
           INSPECT WS-PNR-RAW REPLACING ALL LOW-VALUES BY SPACE

           MOVE FUNCTION TRIM(WS-LAST-NAME-RAW) TO WS-LAST-NAME-IN
           MOVE FUNCTION TRIM(WS-PNR-RAW) TO WS-PNR-IN

           INSPECT WS-LAST-NAME-IN CONVERTING
               'abcdefghijklmnopqrstuvwxyz'
               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

           INSPECT WS-PNR-IN CONVERTING
               'abcdefghijklmnopqrstuvwxyz'
               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

      * =======================================================
      *           STORE VERIFIED BOOKING IN COMMAREA
      * =======================================================
       STORE-CHECKIN-COMMAREA.
           MOVE 'K' TO WS-CA-STATE
           MOVE PR-NAME TO WS-CA-NAME
           MOVE PR-ORIG TO WS-CA-ORIG
           MOVE PR-DEST TO WS-CA-DEST
           MOVE PR-DEPDATE TO WS-CA-DEPDATE
           MOVE PR-RETDATE TO WS-CA-RETDATE
           MOVE PR-TRIPTYPE TO WS-CA-TRIPTYPE
           MOVE PR-OUT-FLT TO WS-CA-OUT-FLT
           MOVE PR-OUT-DEP TO WS-CA-OUT-DEP
           MOVE PR-OUT-ARR TO WS-CA-OUT-ARR
           MOVE PR-RET-FLT TO WS-CA-RET-FLT
           MOVE PR-RET-DEP TO WS-CA-RET-DEP
           MOVE PR-RET-ARR TO WS-CA-RET-ARR
           MOVE WS-PNR-IN TO WS-CA-PNR.

      * =======================================================
      *              POPULATE FLIGHT DISPLAY
      * =======================================================
       POPULATE-FLIGHT-DISPLAY.
           PERFORM POPULATE-DEPARTURE-SECTION
           IF PR-TRIPTYPE = 'R' OR PR-TRIPTYPE = 'r'
               PERFORM POPULATE-RETURN-SECTION
           ELSE
               PERFORM HIDE-RETURN-SECTION
           END-IF
           MOVE 'PRESS ENTER AGAIN FOR SEAT SELECTION' TO MSGO.

      * =======================================================
      *              POPULATE DEPARTURE SECTION
      * =======================================================
       POPULATE-DEPARTURE-SECTION.
           MOVE 'DEPARTURE' TO DEPARTO
           PERFORM FORMAT-DATE-DEP
           MOVE WS-FORMATTED-DATE TO DEPDATEO
           MOVE PR-ORIG TO DEPORGO
           MOVE ' -> ' TO DEPSEPO
           MOVE PR-DEST TO DEPDSTO
           MOVE 'DEP TIME' TO DEPTMLO
           PERFORM FORMAT-TIME-OUTDEP
           MOVE WS-TIME-FORMATTED TO DEPTIMEO
           MOVE 'ARR TIME' TO DEPARRLO
           PERFORM FORMAT-TIME-OUTARR
           MOVE WS-TIME-FORMATTED TO DEPARRO.

      * =======================================================
      *               POPULATE RETURN SECTION
      * =======================================================
       POPULATE-RETURN-SECTION.
           MOVE 'RETURN' TO RETURNO
           PERFORM FORMAT-DATE-RET
           MOVE WS-FORMATTED-DATE TO RETDATEO
           MOVE PR-DEST TO RETORGO
           MOVE ' -> ' TO RETSEPO
           MOVE PR-ORIG TO RETDSTO
           MOVE 'DEP TIME' TO RETTMLO
           PERFORM FORMAT-TIME-RETDEP
           MOVE WS-TIME-FORMATTED TO RETTIMEO
           MOVE 'ARR TIME' TO RETARRLO
           PERFORM FORMAT-TIME-RETARR
           MOVE WS-TIME-FORMATTED TO RETARRO.

      * =======================================================
      *                 SEND CHECK-IN MAP
      * =======================================================
       SEND-CHECKIN-SCREEN.
           EXEC CICS
               SEND MAP('CKINMAP') MAPSET('CKINMAP')
               FROM(CKINMAPO)
               ERASE
           END-EXEC.

      * =======================================================
      *              HIDE FLIGHT SECTIONS
      * =======================================================
       HIDE-FLIGHT-SECTIONS.
           PERFORM HIDE-DEPARTURE-SECTION
           PERFORM HIDE-RETURN-SECTION.

      * =======================================================
      *              HIDE DEPARTURE SECTION
      * =======================================================
       HIDE-DEPARTURE-SECTION.
           MOVE SPACES TO DEPARTO
           MOVE SPACES TO DEPDATEO
           MOVE SPACES TO DEPORGO
           MOVE SPACES TO DEPSEPO
           MOVE SPACES TO DEPDSTO
           MOVE SPACES TO DEPTMLO
           MOVE SPACES TO DEPTIMEO
           MOVE SPACES TO DEPARRLO
           MOVE SPACES TO DEPARRO.

      * =======================================================
      *              HIDE RETURN SECTION
      * =======================================================
       HIDE-RETURN-SECTION.
           MOVE SPACES TO RETURNO
           MOVE SPACES TO RETDATEO
           MOVE SPACES TO RETORGO
           MOVE SPACES TO RETSEPO
           MOVE SPACES TO RETDSTO
           MOVE SPACES TO RETTMLO
           MOVE SPACES TO RETTIMEO
           MOVE SPACES TO RETARRLO
           MOVE SPACES TO RETARRO.

      * =======================================================
      *                  DATE FORMATTING
      * =======================================================
       FORMAT-DATE-DEP.
           IF PR-DEPDATE NOT = SPACES AND PR-DEPDATE NOT = LOW-VALUES
               IF PR-DEPDATE(1:4) IS NUMERIC
                  AND PR-DEPDATE(5:2) IS NUMERIC
                  AND PR-DEPDATE(7:2) IS NUMERIC
                   MOVE PR-DEPDATE(5:2) TO WS-MONTH-NUM
               ELSE
                   MOVE 0 TO WS-MONTH-NUM
               END-IF
               IF WS-MONTH-NUM >= 1 AND WS-MONTH-NUM <= 12
                   STRING WS-MONTH-NAME(WS-MONTH-NUM) ' '
                          PR-DEPDATE(7:2) ' '
                          PR-DEPDATE(1:4)
                       DELIMITED BY SIZE INTO WS-FORMATTED-DATE
                   END-STRING
               ELSE
                   MOVE SPACES TO WS-FORMATTED-DATE
               END-IF
           ELSE
               MOVE SPACES TO WS-FORMATTED-DATE
           END-IF.

       FORMAT-DATE-RET.
           IF PR-RETDATE NOT = SPACES AND PR-RETDATE NOT = LOW-VALUES
               IF PR-RETDATE(1:4) IS NUMERIC
                  AND PR-RETDATE(5:2) IS NUMERIC
                  AND PR-RETDATE(7:2) IS NUMERIC
                   MOVE PR-RETDATE(5:2) TO WS-MONTH-NUM
               ELSE
                   MOVE 0 TO WS-MONTH-NUM
               END-IF
               IF WS-MONTH-NUM >= 1 AND WS-MONTH-NUM <= 12
                   STRING WS-MONTH-NAME(WS-MONTH-NUM) ' '
                          PR-RETDATE(7:2) ' '
                          PR-RETDATE(1:4)
                       DELIMITED BY SIZE INTO WS-FORMATTED-DATE
                   END-STRING
               ELSE
                   MOVE SPACES TO WS-FORMATTED-DATE
               END-IF
           ELSE
               MOVE SPACES TO WS-FORMATTED-DATE
           END-IF.

      * =======================================================
      *                  TIME FORMATTING
      * =======================================================
       FORMAT-TIME-OUTDEP.
           IF PR-OUT-DEP NOT = SPACES AND PR-OUT-DEP NOT = LOW-VALUES
               STRING PR-OUT-DEP(1:2) ':' PR-OUT-DEP(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

       FORMAT-TIME-OUTARR.
           IF PR-OUT-ARR NOT = SPACES AND PR-OUT-ARR NOT = LOW-VALUES
               STRING PR-OUT-ARR(1:2) ':' PR-OUT-ARR(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

       FORMAT-TIME-RETDEP.
           IF PR-RET-DEP NOT = SPACES AND PR-RET-DEP NOT = LOW-VALUES
               STRING PR-RET-DEP(1:2) ':' PR-RET-DEP(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

       FORMAT-TIME-RETARR.
           IF PR-RET-ARR NOT = SPACES AND PR-RET-ARR NOT = LOW-VALUES
               STRING PR-RET-ARR(1:2) ':' PR-RET-ARR(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

      * =======================================================
      *                  RETURN TO CKIN
      * =======================================================
       RETURN-TO-CKIN.
           EXEC CICS
               RETURN TRANSID('CKIN')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(92)
           END-EXEC.

      * =======================================================
      *                 ERROR FALLBACK
      * =======================================================
       SEND-ERROR-MESSAGE.
           EXEC CICS SEND TEXT
               FROM(WS-ERROR-MESSAGE)
               LENGTH(35)
               ERASE
           END-EXEC.