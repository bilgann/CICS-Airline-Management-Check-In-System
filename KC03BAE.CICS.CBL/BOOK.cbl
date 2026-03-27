       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY BOOKMAP.

       01 WS-RESP                  PIC S9(8) COMP.
       01 WS-STATE                 PIC X VALUE SPACE.
       01 WS-ABSTIME               PIC S9(15) COMP-3.
       01 WS-CURR-DATE             PIC X(8).
       01 WS-CURR-TIME             PIC X(6).
       01 WS-PNR-RETRY             PIC 99 VALUE 0.
       01 WS-PNR-WRITTEN           PIC X VALUE 'N'.
       01 WS-LAST-NAME             PIC X(16).
       01 WS-TEMP-NAME             PIC X(18).
       01 WS-NAME-LEN              PIC S9(4) COMP VALUE 0.
       01 WS-LAST-START            PIC S9(4) COMP VALUE 1.
       01 WS-LAST-IX               PIC S9(4) COMP VALUE 0.

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

      * Date conversion work fields
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
       01 WS-FORMATTED-DATE        PIC X(11).
       01 WS-TIME-FORMATTED        PIC X(5).
       01 WS-ERROR-MESSAGE         PIC X(35)
           VALUE 'BOOK: INVALID CALL - NO COMMAREA'.
       01 WS-SAVE-ERROR            PIC X(35)
           VALUE 'BOOK: UNABLE TO SAVE BOOKING DATA'.
       01 WS-RESP-DISP             PIC 9(8).

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

           IF EIBCALEN = 0
               PERFORM SEND-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF.

           EVALUATE CA-STATE
               WHEN 'B'
      *            First entry from FLSR: show BOOK review screen only
                   PERFORM DISPLAY-CONFIRMATION
                   MOVE 'P' TO CA-STATE
                   EXEC CICS RETURN TRANSID('BOOK')
                       COMMAREA(DFHCOMMAREA)
                       LENGTH(92)
                   END-EXEC

               WHEN 'P'
      *            PF1 from BOOK screen: go back to FLSR to change flights
                   IF EIBAID = DFHPF1
                       IF CA-TRIPTYPE = 'R'
                           MOVE 'R' TO CA-STATE
                       ELSE
                           MOVE 'O' TO CA-STATE
                       END-IF

                       EXEC CICS RETURN TRANSID('FLSR')
                           COMMAREA(DFHCOMMAREA)
                           LENGTH(92)
                       END-EXEC
                   END-IF

      *            First ENTER on BOOK confirms flights and creates PNR
                   IF EIBAID = DFHENTER
                       PERFORM SAVE-BOOKING-DETAILS
                       IF WS-PNR-WRITTEN = 'Y'
                           MOVE 'C' TO CA-STATE
                       END-IF
                   END-IF

                   PERFORM DISPLAY-CONFIRMATION
                   EXEC CICS RETURN TRANSID('BOOK')
                       COMMAREA(DFHCOMMAREA)
                       LENGTH(92)
                   END-EXEC

               WHEN 'C'
      *            Second ENTER on BOOK moves user to CKIN
                   IF EIBAID = DFHENTER
                       EXEC CICS
                           XCTL PROGRAM('CKIN')
                                COMMAREA(DFHCOMMAREA)
                                LENGTH(92)
                       END-EXEC
                   END-IF

                   PERFORM DISPLAY-CONFIRMATION
                   EXEC CICS RETURN TRANSID('BOOK')
                       COMMAREA(DFHCOMMAREA)
                       LENGTH(92)
                   END-EXEC

               WHEN OTHER
                   PERFORM SEND-ERROR-MESSAGE
                   EXEC CICS RETURN END-EXEC
           END-EVALUATE.

      * =======================================================
      *           DISPLAY BOOKING CONFIRMATION
      * =======================================================
       DISPLAY-CONFIRMATION.
           MOVE LOW-VALUES TO BOOKMAPO.

      *    Populate name and passport
           MOVE CA-NAME TO NAMEVO
           MOVE CA-PASSPORT TO PASSVO.

      *    Format and display departure date
           PERFORM FORMAT-DATE-DEPDATE
           MOVE WS-FORMATTED-DATE TO DEPDATEO.
           MOVE CA-OUT-FLT TO DEPFLTVO.

      *    Populate departure flight info
           MOVE CA-ORIG TO DEPORGO
           MOVE CA-DEST TO DEPDSTO
           PERFORM FORMAT-TIME-OUTDEP
           MOVE WS-TIME-FORMATTED TO DEPTIMEO
           PERFORM FORMAT-TIME-OUTARR
           MOVE WS-TIME-FORMATTED TO DEPARRO.

      *    Handle return flight if round trip
           IF CA-TRIPTYPE = 'R'
      *        Format and display return date
               PERFORM FORMAT-DATE-RETDATE
               MOVE WS-FORMATTED-DATE TO RETDATEO
               MOVE CA-RET-FLT TO RETFLTVO

      *        Populate return flight info (reverse route)
               MOVE CA-DEST TO RETORGO
               MOVE CA-ORIG TO RETDSTO
               PERFORM FORMAT-TIME-RETDEP
               MOVE WS-TIME-FORMATTED TO RETTIMEO
               PERFORM FORMAT-TIME-RETARR
               MOVE WS-TIME-FORMATTED TO RETARRO
           ELSE
      *        Hide return section for one-way
               PERFORM HIDE-RETURN-SECTION
           END-IF.

           EVALUATE CA-STATE
               WHEN 'B'
                   MOVE 'PRESS ENTER TO CONFIRM BOOKING' TO MSGO
               WHEN 'P'
                   IF EIBAID = DFHENTER AND WS-PNR-WRITTEN = 'N'
                       MOVE WS-SAVE-ERROR TO MSGO
                   ELSE
                       MOVE 'PRESS ENTER TO CONFIRM BOOKING' TO MSGO
                   END-IF
               WHEN 'C'
                   STRING 'PNR ' CA-PNR
                          ' CONFIRMED - PRESS ENTER FOR CKIN'
                       DELIMITED BY SIZE INTO MSGO
                   END-STRING
               WHEN OTHER
                   MOVE SPACES TO MSGO
           END-EVALUATE.

      *    Send the map
           EXEC CICS
               SEND MAP('BOOKMAP') MAPSET('BOOKMAP')
               FROM(BOOKMAPO)
               ERASE
           END-EXEC.

      * =======================================================
      *          SAVE BOOKING AND GENERATE PNR
      * =======================================================
       SAVE-BOOKING-DETAILS.
           MOVE 'Y' TO WS-PNR-WRITTEN.

           IF CA-PNR = SPACES
               PERFORM GENERATE-PNR
           END-IF

           PERFORM BUILD-PNR-RECORD

           EXEC CICS
               READ FILE('PNRDATA')
                    INTO(WS-PNR-REC)
                    RIDFLD(CA-PNR)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM BUILD-PNR-RECORD
                   EXEC CICS
                       REWRITE FILE('PNRDATA')
                               FROM(WS-PNR-REC)
                               RESP(WS-RESP)
                   END-EXEC
               WHEN DFHRESP(NOTFND)
                   EXEC CICS
                       WRITE FILE('PNRDATA')
                             FROM(WS-PNR-REC)
                             RIDFLD(CA-PNR)
                             RESP(WS-RESP)
                   END-EXEC
               WHEN OTHER
                   PERFORM SET-SAVE-ERROR
                   MOVE 'N' TO WS-PNR-WRITTEN
           END-EVALUATE

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM SET-SAVE-ERROR
               MOVE 'N' TO WS-PNR-WRITTEN
           END-IF.

       GENERATE-PNR.
           MOVE 0 TO WS-PNR-RETRY

           PERFORM UNTIL WS-PNR-RETRY > 9
               EXEC CICS
                   ASKTIME ABSTIME(WS-ABSTIME)
               END-EXEC

               EXEC CICS
                   FORMATTIME ABSTIME(WS-ABSTIME)
                              YYYYMMDD(WS-CURR-DATE)
                              TIME(WS-CURR-TIME)
               END-EXEC

               STRING WS-CURR-DATE(5:2)
                      WS-CURR-TIME(3:2)
                      WS-CURR-TIME(5:2)
                   DELIMITED BY SIZE INTO CA-PNR
               END-STRING

               IF WS-PNR-RETRY > 0
                   MOVE WS-PNR-RETRY TO CA-PNR(6:1)
               END-IF

               EXEC CICS
                   READ FILE('PNRDATA')
                        INTO(WS-PNR-REC)
                        RIDFLD(CA-PNR)
                        RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(NOTFND)
                   EXIT PERFORM
               END-IF

               IF WS-RESP NOT = DFHRESP(NORMAL)
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-PNR-RETRY
           END-PERFORM.

       SET-SAVE-ERROR.
           MOVE SPACES TO WS-SAVE-ERROR

           EVALUATE WS-RESP
               WHEN DFHRESP(FILENOTFOUND)
                   MOVE 'BOOK: PNRDATA FILE NOT DEFINED'
                       TO WS-SAVE-ERROR
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'BOOK: PNRDATA FILE IS CLOSED'
                       TO WS-SAVE-ERROR
               WHEN OTHER
                   MOVE WS-RESP TO WS-RESP-DISP
                   STRING 'SAVE ERR RESP=' WS-RESP-DISP
                       DELIMITED BY SIZE
                       INTO WS-SAVE-ERROR
           END-EVALUATE.

       BUILD-PNR-RECORD.
           MOVE SPACES TO WS-PNR-REC
           PERFORM EXTRACT-LAST-NAME
           MOVE CA-PNR TO PR-PNR
           MOVE WS-LAST-NAME TO PR-LAST-NAME
           MOVE CA-NAME TO PR-NAME
           MOVE CA-ORIG TO PR-ORIG
           MOVE CA-DEST TO PR-DEST
           MOVE CA-DEPDATE TO PR-DEPDATE
           MOVE CA-RETDATE TO PR-RETDATE
           MOVE CA-TRIPTYPE TO PR-TRIPTYPE
           MOVE CA-OUT-FLT TO PR-OUT-FLT
           MOVE CA-OUT-DEP TO PR-OUT-DEP
           MOVE CA-OUT-ARR TO PR-OUT-ARR
           MOVE CA-RET-FLT TO PR-RET-FLT
           MOVE CA-RET-DEP TO PR-RET-DEP
           MOVE CA-RET-ARR TO PR-RET-ARR.

       EXTRACT-LAST-NAME.
           MOVE SPACES TO WS-LAST-NAME
           MOVE FUNCTION TRIM(CA-NAME) TO WS-TEMP-NAME
           MOVE 1 TO WS-LAST-START

           MOVE LENGTH OF WS-TEMP-NAME TO WS-NAME-LEN

           PERFORM VARYING WS-NAME-LEN FROM WS-NAME-LEN BY -1
               UNTIL WS-NAME-LEN < 1
                  OR WS-TEMP-NAME(WS-NAME-LEN:1) NOT = SPACE
           END-PERFORM

           IF WS-NAME-LEN > 0
               PERFORM VARYING WS-LAST-IX FROM WS-NAME-LEN BY -1
                   UNTIL WS-LAST-IX < 1
                   IF WS-TEMP-NAME(WS-LAST-IX:1) = SPACE
                       COMPUTE WS-LAST-START = WS-LAST-IX + 1
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-LAST-START <= WS-NAME-LEN
                   MOVE WS-TEMP-NAME(WS-LAST-START:
                       WS-NAME-LEN - WS-LAST-START + 1)
                       TO WS-LAST-NAME
               END-IF
           END-IF

           INSPECT WS-LAST-NAME CONVERTING
               'abcdefghijklmnopqrstuvwxyz'
               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

      * =======================================================
      *              HIDE RETURN SECTION
      * =======================================================
       HIDE-RETURN-SECTION.
           MOVE SPACES TO RETURNO
           MOVE SPACES TO RETDATEO
           MOVE SPACES TO RETFLTLO
           MOVE SPACES TO RETFLTVO
           MOVE SPACES TO RETSEP1O
           MOVE SPACES TO RETORGO
           MOVE SPACES TO RETSEPO
           MOVE SPACES TO RETDSTO
           MOVE SPACES TO RETTMLO
           MOVE SPACES TO RETTIMEO
           MOVE SPACES TO RETARRLO
           MOVE SPACES TO RETARRO
           MOVE SPACES TO RETSEP2O.

      * =======================================================
      *              FORMAT DEPARTURE DATE
      * =======================================================
       FORMAT-DATE-DEPDATE.
      *    Input format: YYYYMMDD, Output: MMM DD YYYY
           IF CA-DEPDATE NOT = SPACES AND CA-DEPDATE NOT = LOW-VALUES
               MOVE CA-DEPDATE(5:2) TO WS-MONTH-NUM
               IF WS-MONTH-NUM >= 1 AND WS-MONTH-NUM <= 12
                   STRING WS-MONTH-NAME(WS-MONTH-NUM) ' '
                          CA-DEPDATE(7:2) ' '
                          CA-DEPDATE(1:4)
                       DELIMITED BY SIZE INTO WS-FORMATTED-DATE
                   END-STRING
               ELSE
                   MOVE SPACES TO WS-FORMATTED-DATE
               END-IF
           ELSE
               MOVE SPACES TO WS-FORMATTED-DATE
           END-IF.

      * =======================================================
      *              FORMAT RETURN DATE
      * =======================================================
       FORMAT-DATE-RETDATE.
      *    Input format: YYYYMMDD, Output: MMM DD YYYY
           IF CA-RETDATE NOT = SPACES AND CA-RETDATE NOT = LOW-VALUES
               MOVE CA-RETDATE(5:2) TO WS-MONTH-NUM
               IF WS-MONTH-NUM >= 1 AND WS-MONTH-NUM <= 12
                   STRING WS-MONTH-NAME(WS-MONTH-NUM) ' '
                          CA-RETDATE(7:2) ' '
                          CA-RETDATE(1:4)
                       DELIMITED BY SIZE INTO WS-FORMATTED-DATE
                   END-STRING
               ELSE
                   MOVE SPACES TO WS-FORMATTED-DATE
               END-IF
           ELSE
               MOVE SPACES TO WS-FORMATTED-DATE
           END-IF.

      * =======================================================
      *              FORMAT TIME FIELDS
      * =======================================================
       FORMAT-TIME-OUTDEP.
      *    Input: HHMM (4 digits), Output: HH:MM
           IF CA-OUT-DEP NOT = SPACES AND CA-OUT-DEP NOT = LOW-VALUES
               STRING CA-OUT-DEP(1:2) ':' CA-OUT-DEP(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

       FORMAT-TIME-OUTARR.
           IF CA-OUT-ARR NOT = SPACES AND CA-OUT-ARR NOT = LOW-VALUES
               STRING CA-OUT-ARR(1:2) ':' CA-OUT-ARR(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

       FORMAT-TIME-RETDEP.
           IF CA-RET-DEP NOT = SPACES AND CA-RET-DEP NOT = LOW-VALUES
               STRING CA-RET-DEP(1:2) ':' CA-RET-DEP(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

       FORMAT-TIME-RETARR.
           IF CA-RET-ARR NOT = SPACES AND CA-RET-ARR NOT = LOW-VALUES
               STRING CA-RET-ARR(1:2) ':' CA-RET-ARR(3:2)
                   DELIMITED BY SIZE INTO WS-TIME-FORMATTED
               END-STRING
           ELSE
               MOVE SPACES TO WS-TIME-FORMATTED
           END-IF.

      * =======================================================
      *              SEND ERROR MESSAGE
      * =======================================================
       SEND-ERROR-MESSAGE.
           EXEC CICS SEND TEXT
               FROM(WS-ERROR-MESSAGE)
               LENGTH(35)
               ERASE
           END-EXEC.
