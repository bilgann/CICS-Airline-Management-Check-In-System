       IDENTIFICATION DIVISION.
       PROGRAM-ID. CKIN2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY CKI2MAP.

       01 WS-RESP                  PIC S9(8) COMP.
       01 WS-TRANSID               PIC X(4) VALUE 'CKI2'.
       01 WS-CURRENT-FLIGHT        PIC X(6).
       01 WS-CURRENT-SEGMENT       PIC X(10).
       01 WS-MESSAGE               PIC X(70).
       01 WS-SELECTED-SEAT         PIC X(3).
       01 WS-SEAT-INPUT-RAW        PIC X(3).
       01 WS-SEAT-MATCH            PIC X VALUE 'N'.
       01 WS-SEAT-INDEX            PIC 99 VALUE 0.
       01 WS-I                     PIC 99 VALUE 0.

      * Seat file record: flight number key plus 9 available seat slots.
       01 WS-SEAT-REC.
           05 SR-FLIGHT            PIC X(6).
           05 SR-SEAT OCCURS 9 TIMES.
               10 SR-SEATNO        PIC X(3).

       01 WS-SEAT-KEY              PIC X(6).

       01 WS-SEAT-LINES.
           05 WS-LINE-1            PIC X(20).
           05 WS-LINE-2            PIC X(20).
           05 WS-LINE-3            PIC X(20).

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
           05 CA-BAGGAGE           PIC X(1).
           05 CA-HANDLUGGAGE       PIC X(1).
           05 CA-OUT-SEAT          PIC X(3).
           05 CA-RET-SEAT          PIC X(3).
           05 CA-OUT-BAGGAGE       PIC X(1).
           05 CA-OUT-CARRYON       PIC X(1).
           05 CA-RET-BAGGAGE       PIC X(1).
           05 CA-RET-CARRYON       PIC X(1).

       PROCEDURE DIVISION USING DFHCOMMAREA.

      * =======================================================
      *                   MAIN LOGIC
      * =======================================================
       MAIN-SECTION.
           IF EIBCALEN = 0
               EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF3
               EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF1
               EXEC CICS
                   XCTL PROGRAM('CKIN')
                        COMMAREA(DFHCOMMAREA)
                        LENGTH(104)
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF

           IF CA-STATE NOT = 'D'
              AND CA-STATE NOT = 'R'
              AND CA-STATE NOT = 'C'
               MOVE 'D' TO CA-STATE
               MOVE SPACES TO WS-MESSAGE
               MOVE SPACES TO CA-OUT-SEAT
               MOVE SPACES TO CA-RET-SEAT
               MOVE SPACES TO CA-OUT-BAGGAGE
               MOVE SPACES TO CA-OUT-CARRYON
               MOVE SPACES TO CA-RET-BAGGAGE
               MOVE SPACES TO CA-RET-CARRYON
               PERFORM DISPLAY-STAGE-SCREEN
               PERFORM RETURN-TO-CKIN2
           END-IF

           EXEC CICS
               RECEIVE MAP('CKI2MAP') MAPSET('CKI2MAP')
               INTO(CKI2MAPI)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE SPACES TO WS-MESSAGE
               PERFORM DISPLAY-STAGE-SCREEN
               PERFORM RETURN-TO-CKIN2
           END-IF

           EVALUATE CA-STATE
               WHEN 'D'
                   PERFORM PROCESS-DEPARTURE-SEAT
               WHEN 'R'
                   PERFORM PROCESS-RETURN-SEAT
               WHEN 'C'
                   PERFORM DISPLAY-COMPLETE-SCREEN
                   PERFORM TRANSFER-TO-CKIN3
               WHEN OTHER
                   MOVE 'D' TO CA-STATE
                   MOVE SPACES TO WS-MESSAGE
                   PERFORM DISPLAY-STAGE-SCREEN
           END-EVALUATE

           IF CA-STATE = 'C'
               PERFORM TRANSFER-TO-CKIN3
           ELSE
               PERFORM RETURN-TO-CKIN2
           END-IF.

      * =======================================================
      *            PROCESS DEPARTURE SEAT SELECTION
      * =======================================================
       PROCESS-DEPARTURE-SEAT.
           PERFORM NORMALIZE-SEAT-INPUT

           IF WS-SELECTED-SEAT = SPACES
               MOVE 'ENTER A SEAT NUMBER.' TO WS-MESSAGE
               PERFORM DISPLAY-STAGE-SCREEN
               EXIT PARAGRAPH
           END-IF

           MOVE CA-OUT-FLT TO WS-SEAT-KEY
           PERFORM LOAD-SEAT-RECORD

           IF WS-RESP NOT = DFHRESP(NORMAL)
               STRING 'NO SEAT DATA FOUND FOR FLIGHT '
                      CA-OUT-FLT
                   DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
               PERFORM DISPLAY-STAGE-SCREEN
               EXIT PARAGRAPH
           END-IF

           PERFORM VALIDATE-SEAT-SELECTION

           IF WS-SEAT-MATCH NOT = 'Y'
               STRING 'SEAT ' WS-SELECTED-SEAT
                      ' IS NOT AVAILABLE FOR FLIGHT ' CA-OUT-FLT
                   DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
               PERFORM DISPLAY-STAGE-SCREEN
               EXIT PARAGRAPH
           END-IF

           IF CA-TRIPTYPE = 'R' OR CA-TRIPTYPE = 'r'
               MOVE WS-SELECTED-SEAT TO CA-OUT-SEAT
               MOVE 'R' TO CA-STATE
               STRING 'DEPARTURE SEAT ' WS-SELECTED-SEAT
                      ' SAVED. SELECT RETURN SEAT.'
                   DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
           ELSE
               MOVE WS-SELECTED-SEAT TO CA-OUT-SEAT
               MOVE SPACES TO CA-RET-SEAT
               MOVE 'C' TO CA-STATE
               STRING 'CHECK-IN COMPLETE. FLIGHT ' CA-OUT-FLT
                      ' SEAT ' WS-SELECTED-SEAT ' CONFIRMED.'
                   DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
           END-IF

           PERFORM DISPLAY-STAGE-SCREEN.

      * =======================================================
      *             PROCESS RETURN SEAT SELECTION
      * =======================================================
       PROCESS-RETURN-SEAT.
           PERFORM NORMALIZE-SEAT-INPUT

           IF WS-SELECTED-SEAT = SPACES
               MOVE 'ENTER A SEAT NUMBER.' TO WS-MESSAGE
               PERFORM DISPLAY-STAGE-SCREEN
               EXIT PARAGRAPH
           END-IF

           MOVE CA-RET-FLT TO WS-SEAT-KEY
           PERFORM LOAD-SEAT-RECORD

           IF WS-RESP NOT = DFHRESP(NORMAL)
               STRING 'NO SEAT DATA FOUND FOR FLIGHT '
                      CA-RET-FLT
                   DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
               PERFORM DISPLAY-STAGE-SCREEN
               EXIT PARAGRAPH
           END-IF

           PERFORM VALIDATE-SEAT-SELECTION

           IF WS-SEAT-MATCH NOT = 'Y'
               STRING 'SEAT ' WS-SELECTED-SEAT
                      ' IS NOT AVAILABLE FOR FLIGHT ' CA-RET-FLT
                   DELIMITED BY SIZE INTO WS-MESSAGE
               END-STRING
               PERFORM DISPLAY-STAGE-SCREEN
               EXIT PARAGRAPH
           END-IF

           MOVE 'C' TO CA-STATE
              MOVE WS-SELECTED-SEAT TO CA-RET-SEAT
           STRING 'CHECK-IN COMPLETE. FLIGHT ' CA-RET-FLT
                  ' SEAT ' WS-SELECTED-SEAT ' CONFIRMED.'
               DELIMITED BY SIZE INTO WS-MESSAGE
           END-STRING

           PERFORM DISPLAY-STAGE-SCREEN.

      * =======================================================
      *               DISPLAY CURRENT STAGE
      * =======================================================
       DISPLAY-STAGE-SCREEN.
           MOVE LOW-VALUES TO CKI2MAPO
           PERFORM SET-CURRENT-FLIGHT-CONTEXT

           MOVE CA-PNR TO PNRVO
           MOVE WS-CURRENT-SEGMENT TO SEGMENTO
           MOVE WS-CURRENT-FLIGHT TO FLIGHTNO
           MOVE SPACES TO SEATINO

           PERFORM LOAD-SEAT-RECORD

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM BUILD-SEAT-LINES
               MOVE WS-LINE-1 TO ROW1TXTO
               MOVE WS-LINE-2 TO ROW2TXTO
               MOVE WS-LINE-3 TO ROW3TXTO
               IF WS-MESSAGE = SPACES
                   MOVE 'SELECT AN AVAILABLE SEAT AND PRESS ENTER.' TO
                       MSGO
               ELSE
                   MOVE WS-MESSAGE TO MSGO
               END-IF
           ELSE
               PERFORM CLEAR-SEAT-DISPLAY
               IF WS-MESSAGE = SPACES
                   STRING 'NO SEAT DATA FOUND FOR FLIGHT '
                          WS-CURRENT-FLIGHT
                       DELIMITED BY SIZE INTO MSGO
                   END-STRING
               ELSE
                   MOVE WS-MESSAGE TO MSGO
               END-IF
           END-IF

           EXEC CICS
               SEND MAP('CKI2MAP') MAPSET('CKI2MAP')
               FROM(CKI2MAPO)
               ERASE
           END-EXEC.

      * =======================================================
      *                DISPLAY COMPLETE SCREEN
      * =======================================================
       DISPLAY-COMPLETE-SCREEN.
           IF WS-MESSAGE = SPACES
               MOVE 'PRESS ENTER FOR BAGGAGE SELECTION.' TO
                   WS-MESSAGE
           END-IF
           PERFORM DISPLAY-STAGE-SCREEN.

      * =======================================================      *
      * =======================================================
       TRANSFER-TO-CKIN3.
           EXEC CICS
               XCTL PROGRAM('CKIN3')
                    COMMAREA(DFHCOMMAREA)
                    LENGTH(104)
           END-EXEC.

      * =======================================================      *
      * =======================================================
       SET-CURRENT-FLIGHT-CONTEXT.
           IF CA-STATE = 'R'
               MOVE 'RETURN' TO WS-CURRENT-SEGMENT
               MOVE CA-RET-FLT TO WS-CURRENT-FLIGHT
           ELSE
               IF (CA-STATE = 'C') AND
                  (CA-TRIPTYPE = 'R' OR CA-TRIPTYPE = 'r') AND
                  CA-RET-FLT NOT = SPACES
                   MOVE 'RETURN' TO WS-CURRENT-SEGMENT
                   MOVE CA-RET-FLT TO WS-CURRENT-FLIGHT
               ELSE
                   MOVE 'DEPARTURE' TO WS-CURRENT-SEGMENT
                   MOVE CA-OUT-FLT TO WS-CURRENT-FLIGHT
               END-IF
           END-IF.

           MOVE WS-CURRENT-FLIGHT TO WS-SEAT-KEY.

      * =======================================================
      *                READ SEAT RECORD
      * =======================================================
       LOAD-SEAT-RECORD.
           MOVE LOW-VALUES TO WS-SEAT-REC

           EXEC CICS
               READ FILE('SEATFIL')
                    INTO(WS-SEAT-REC)
                    RIDFLD(WS-SEAT-KEY)
                    RESP(WS-RESP)
           END-EXEC.

      * =======================================================
      *          READ SEAT RECORD FOR UPDATE/REWRITE
      * =======================================================
       LOAD-SEAT-RECORD-UPDATE.
           MOVE LOW-VALUES TO WS-SEAT-REC

           EXEC CICS
               READ FILE('SEATFIL')
                    INTO(WS-SEAT-REC)
                    RIDFLD(WS-SEAT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC.

      * =======================================================
      *                BUILD DISPLAY LINES
      * =======================================================
       BUILD-SEAT-LINES.
           MOVE SPACES TO WS-SEAT-LINES

           STRING SR-SEATNO(1) '  ' SR-SEATNO(2) '  ' SR-SEATNO(3)
               DELIMITED BY SIZE INTO WS-LINE-1
           END-STRING

           STRING SR-SEATNO(4) '  ' SR-SEATNO(5) '  ' SR-SEATNO(6)
               DELIMITED BY SIZE INTO WS-LINE-2
           END-STRING

           STRING SR-SEATNO(7) '  ' SR-SEATNO(8) '  ' SR-SEATNO(9)
               DELIMITED BY SIZE INTO WS-LINE-3
           END-STRING.

      * =======================================================
      *                 NORMALIZE INPUT
      * =======================================================
       NORMALIZE-SEAT-INPUT.
           MOVE SPACES TO WS-SELECTED-SEAT
           MOVE SEATINI TO WS-SEAT-INPUT-RAW
           INSPECT WS-SEAT-INPUT-RAW REPLACING ALL LOW-VALUES BY SPACE
           MOVE FUNCTION TRIM(WS-SEAT-INPUT-RAW) TO WS-SELECTED-SEAT

           INSPECT WS-SELECTED-SEAT CONVERTING
               'abcdefghijklmnopqrstuvwxyz'
               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

      * =======================================================
      *              VALIDATE SELECTED SEAT
      * =======================================================
       VALIDATE-SEAT-SELECTION.
           MOVE 'N' TO WS-SEAT-MATCH
           MOVE 0 TO WS-SEAT-INDEX

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               IF SR-SEATNO(WS-I) = WS-SELECTED-SEAT
                   MOVE 'Y' TO WS-SEAT-MATCH
                   MOVE WS-I TO WS-SEAT-INDEX
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      * =======================================================
      *               CLEAR DISPLAYED SEATS
      * =======================================================
       CLEAR-SEAT-DISPLAY.
           MOVE SPACES TO ROW1TXTO
           MOVE SPACES TO ROW2TXTO
           MOVE SPACES TO ROW3TXTO.

      * =======================================================
      *                 RETURN TO TRANSID
      * =======================================================
       RETURN-TO-CKIN2.
           EXEC CICS
               RETURN TRANSID(WS-TRANSID)
                      COMMAREA(DFHCOMMAREA)
                      LENGTH(104)
           END-EXEC.
