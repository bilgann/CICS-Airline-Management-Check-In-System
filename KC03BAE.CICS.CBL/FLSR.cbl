       IDENTIFICATION DIVISION.
       PROGRAM-ID. FLSR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.

       01 WS-RESP                  PIC S9(8) COMP.
       01 WS-AID                   PIC X.
       01 WS-VALID-FLAG            PIC X VALUE 'Y'.
       01 WS-STATE                 PIC X VALUE SPACE.
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
       01 WS-I                     PIC 9(4) COMP VALUE 0.
       01 WS-FILE-STATUS           PIC X(2) VALUE '00'.
       01 WS-VSAM-REC              PIC X(38).
       01 WS-TRIPTYPE              PIC X VALUE SPACE.
       01 WS-SELECTION-MADE        PIC X VALUE 'N'.
       01 WS-SELECTED-ROW          PIC 9(2) VALUE 0.

       01 WS-KEY.
           05 WS-ORIG              PIC X(3) VALUE SPACE.
           05 WS-DATE              PIC X(8) VALUE SPACE.
           05 WS-DEST              PIC X(3) VALUE SPACE.
           05 WS-FLTNUM            PIC X(5) VALUE SPACE.

       01 WS-REC.
           05 WR-FLTNUM            PIC X(5) VALUE SPACE.
           05 WR-ORIG              PIC X(3) VALUE SPACE.
           05 WR-DATE              PIC X(8) VALUE SPACE.
           05 WR-DEST              PIC X(3) VALUE SPACE.
           05 WR-DEPTIME           PIC X(4) VALUE SPACE.
           05 FILLER               PIC X(8) VALUE SPACE. *> ARRDATE
           05 WR-ARRTIME           PIC X(4) VALUE SPACE.
           05 WR-SEATS             PIC X(3) VALUE SPACE.

       01 WS-ROW-IX                PIC S9(4) COMP VALUE 1.
       01 WS-EOF                   PIC X VALUE 'N'.

      * Array to store displayed flights
       01 WS-FLIGHT-TABLE.
           05 WS-FLIGHT OCCURS 10 TIMES.
               10 WS-FLT-NUM       PIC X(6).
               10 WS-FLT-DEP       PIC X(4).
               10 WS-FLT-ARR       PIC X(4).

       COPY FLSRMAP.


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
      *                   MAIN SCREEN LOGIC
      * =======================================================
       MAIN-SECTION.

      *    Check state from COMMAREA
           IF EIBCALEN = 0
               PERFORM SEND-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           ELSE
               MOVE CA-STATE TO WS-STATE
           END-IF.

           EVALUATE WS-STATE
               WHEN 'S'
      *            Show initial outbound flight selection
                   PERFORM SHOW-OUTBOUND-FLIGHTS
                   MOVE 'O' TO WS-CA-STATE
                   MOVE CA-NAME TO WS-CA-NAME
                   MOVE CA-PASSPORT TO WS-CA-PASSPORT
                   MOVE CA-ORIG TO WS-CA-ORIG
                   MOVE CA-DEST TO WS-CA-DEST
                   MOVE CA-DEPDATE TO WS-CA-DEPDATE
                   MOVE CA-RETDATE TO WS-CA-RETDATE
                   MOVE CA-TRIPTYPE TO WS-CA-TRIPTYPE
                   MOVE CA-PNR TO WS-CA-PNR
                   EXEC CICS
                       RETURN TRANSID('FLSR')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                   END-EXEC

               WHEN 'O'
      *            Process outbound flight selection
                   MOVE CA-NAME TO WS-CA-NAME
                   MOVE CA-PASSPORT TO WS-CA-PASSPORT
                   MOVE CA-ORIG TO WS-CA-ORIG
                   MOVE CA-DEST TO WS-CA-DEST
                   MOVE CA-DEPDATE TO WS-CA-DEPDATE
                   MOVE CA-RETDATE TO WS-CA-RETDATE
                   MOVE CA-TRIPTYPE TO WS-CA-TRIPTYPE
                   MOVE CA-OUT-FLT TO WS-CA-OUT-FLT
                   MOVE CA-OUT-DEP TO WS-CA-OUT-DEP
                   MOVE CA-OUT-ARR TO WS-CA-OUT-ARR
                   MOVE CA-PNR TO WS-CA-PNR
                   PERFORM HANDLE-OUTBOUND-INPUT

               WHEN 'R'
      *            Process return flight selection
                   MOVE CA-NAME TO WS-CA-NAME
                   MOVE CA-PASSPORT TO WS-CA-PASSPORT
                   MOVE CA-ORIG TO WS-CA-ORIG
                   MOVE CA-DEST TO WS-CA-DEST
                   MOVE CA-DEPDATE TO WS-CA-DEPDATE
                   MOVE CA-RETDATE TO WS-CA-RETDATE
                   MOVE CA-TRIPTYPE TO WS-CA-TRIPTYPE
                   MOVE CA-OUT-FLT TO WS-CA-OUT-FLT
                   MOVE CA-OUT-DEP TO WS-CA-OUT-DEP
                   MOVE CA-OUT-ARR TO WS-CA-OUT-ARR
                   MOVE CA-RET-FLT TO WS-CA-RET-FLT
                   MOVE CA-RET-DEP TO WS-CA-RET-DEP
                   MOVE CA-RET-ARR TO WS-CA-RET-ARR
                   MOVE CA-PNR TO WS-CA-PNR
                   PERFORM HANDLE-RETURN-INPUT

               WHEN OTHER
                   PERFORM SEND-ERROR-MESSAGE
                   EXEC CICS RETURN END-EXEC
           END-EVALUATE.

      * =======================================================
      *           SHOW OUTBOUND FLIGHT SELECTION
      * =======================================================
       SHOW-OUTBOUND-FLIGHTS.
           MOVE CA-ORIG TO WS-ORIG
           MOVE CA-DEST TO WS-DEST
           MOVE CA-DEPDATE TO WS-DATE
           MOVE CA-TRIPTYPE TO WS-TRIPTYPE.

           MOVE LOW-VALUES TO FLSRMAPO.
           MOVE 'OUTBOUND FLIGHTS' TO TITLEO
           MOVE WS-ORIG TO ORIGO
           MOVE WS-DEST TO DESTO.

      *    Initialize flight table
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE SPACES TO WS-FLT-NUM(WS-I)
               MOVE SPACES TO WS-FLT-DEP(WS-I)
               MOVE SPACES TO WS-FLT-ARR(WS-I)
           END-PERFORM.

      *    Clear all flight rows before loading
           PERFORM CLEAR-ALL-ROWS

      *    Read VSAM file and filter matching flights
           MOVE 1 TO WS-ROW-IX
           PERFORM READ-VSAM-FLIGHTS.

      *    Display the screen
           EXEC CICS
               SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
               FROM (FLSRMAPO)
               ERASE
           END-EXEC.

      * =======================================================
      *          HANDLE OUTBOUND FLIGHT INPUT
      * =======================================================
       HANDLE-OUTBOUND-INPUT.

      *    Check for PF keys first
           IF EIBAID = DFHPF1
      *        F1 = Go Back to FLTS
               MOVE 'F' TO WS-CA-STATE
               EXEC CICS XCTL
                   PROGRAM('FLTS')
                   COMMAREA(WS-COMMAREA)
                   LENGTH(92)
                   RESP(WS-RESP)
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.

           IF EIBAID = DFHPF3
               EXEC CICS RETURN END-EXEC
           END-IF.

      *    Receive map input
           EXEC CICS
               RECEIVE MAP('FLSRMAP') MAPSET('FLSRMAP')
               INTO(FLSRMAPI)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               PERFORM SHOW-OUTBOUND-FLIGHTS
               IF EIBAID = DFHENTER OR EIBAID = DFHPF2
                   MOVE 'ERROR: SELECT A FLIGHT WITH X' TO MSGO
                   EXEC CICS
                       SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
                       FROM (FLSRMAPO)
                       DATAONLY
                   END-EXEC
               END-IF
               MOVE 'O' TO WS-CA-STATE
               EXEC CICS
                   RETURN TRANSID('FLSR')
                   COMMAREA(WS-COMMAREA)
                   LENGTH(92)
               END-EXEC
           END-IF.

      *    Process AID key
           IF EIBAID = DFHENTER OR EIBAID = DFHPF2
      *        Validate selection before proceeding
               PERFORM VALIDATE-OUTBOUND-SELECTION
               IF WS-SELECTION-MADE = 'Y'
      *            Save selected flight info
                   MOVE WS-FLT-NUM(WS-SELECTED-ROW)
                       TO WS-CA-OUT-FLT
                   MOVE WS-FLT-DEP(WS-SELECTED-ROW)
                       TO WS-CA-OUT-DEP
                   MOVE WS-FLT-ARR(WS-SELECTED-ROW)
                       TO WS-CA-OUT-ARR

                   IF WS-CA-TRIPTYPE = 'R'
      *                Show return flight selection
                       PERFORM SHOW-RETURN-FLIGHTS
                       MOVE 'R' TO WS-CA-STATE
                       EXEC CICS
                           RETURN TRANSID('FLSR')
                           COMMAREA(WS-COMMAREA)
                           LENGTH(92)
                       END-EXEC
                   ELSE
      *                One-way trip - transfer to booking confirmation
                       MOVE 'B' TO WS-CA-STATE
                       EXEC CICS XCTL
                           PROGRAM('BOOK')
                           COMMAREA(WS-COMMAREA)
                           LENGTH(92)
                           RESP(WS-RESP)
                       END-EXEC
                       EXEC CICS RETURN END-EXEC
                   END-IF
               ELSE
      *            No selection - redisplay with error
                   PERFORM SHOW-OUTBOUND-FLIGHTS
                   MOVE 'ERROR: SELECT A FLIGHT WITH X' TO MSGO
                   EXEC CICS
                       SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
                       FROM (FLSRMAPO)
                       DATAONLY
                   END-EXEC
                   MOVE 'O' TO WS-CA-STATE
                   EXEC CICS
                       RETURN TRANSID('FLSR')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                   END-EXEC
               END-IF
           END-IF.

      *    Default - redisplay screen
           PERFORM SHOW-OUTBOUND-FLIGHTS
           MOVE 'O' TO WS-CA-STATE
           EXEC CICS
               RETURN TRANSID('FLSR')
               COMMAREA(WS-COMMAREA)
               LENGTH(92)
           END-EXEC.

      * =======================================================
      *           VALIDATE OUTBOUND SELECTION
      * =======================================================
       VALIDATE-OUTBOUND-SELECTION.
           MOVE 'N' TO WS-SELECTION-MADE
           MOVE 0 TO WS-SELECTED-ROW.

      *    Check each SELECT field
           IF SEL01I = 'X' OR SEL01I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 1 TO WS-SELECTED-ROW
           END-IF.
           IF SEL02I = 'X' OR SEL02I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 2 TO WS-SELECTED-ROW
           END-IF.
           IF SEL03I = 'X' OR SEL03I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 3 TO WS-SELECTED-ROW
           END-IF.
           IF SEL04I = 'X' OR SEL04I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 4 TO WS-SELECTED-ROW
           END-IF.
           IF SEL05I = 'X' OR SEL05I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 5 TO WS-SELECTED-ROW
           END-IF.
           IF SEL06I = 'X' OR SEL06I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 6 TO WS-SELECTED-ROW
           END-IF.
           IF SEL07I = 'X' OR SEL07I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 7 TO WS-SELECTED-ROW
           END-IF.
           IF SEL08I = 'X' OR SEL08I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 8 TO WS-SELECTED-ROW
           END-IF.
           IF SEL09I = 'X' OR SEL09I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 9 TO WS-SELECTED-ROW
           END-IF.
           IF SEL10I = 'X' OR SEL10I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 10 TO WS-SELECTED-ROW
           END-IF.

      * =======================================================
      *           SHOW RETURN FLIGHT SELECTION
      * =======================================================
       SHOW-RETURN-FLIGHTS.
      *    Swap origin and destination for return flight
           MOVE CA-DEST TO WS-ORIG
           MOVE CA-ORIG TO WS-DEST
           MOVE CA-RETDATE TO WS-DATE.

           MOVE LOW-VALUES TO FLSRMAPO.
           MOVE 'RETURN FLIGHTS' TO TITLEO
           MOVE WS-ORIG TO ORIGO
           MOVE WS-DEST TO DESTO.

      *    Initialize flight table
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE SPACES TO WS-FLT-NUM(WS-I)
               MOVE SPACES TO WS-FLT-DEP(WS-I)
               MOVE SPACES TO WS-FLT-ARR(WS-I)
           END-PERFORM.

      *    Clear all flight rows before loading
           PERFORM CLEAR-ALL-ROWS

      *    Read VSAM file and filter matching flights
           MOVE 1 TO WS-ROW-IX
           PERFORM READ-VSAM-FLIGHTS.

      *    Display the screen
           EXEC CICS
               SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
               FROM (FLSRMAPO)
               ERASE
           END-EXEC.

      * =======================================================
      *          HANDLE RETURN FLIGHT INPUT
      * =======================================================
       HANDLE-RETURN-INPUT.

      *    Check for PF keys first
           IF EIBAID = DFHPF1
      *        F1 = Go Back to outbound selection
               PERFORM SHOW-OUTBOUND-FLIGHTS
               MOVE 'O' TO WS-CA-STATE
               EXEC CICS
                   RETURN TRANSID('FLSR')
                   COMMAREA(WS-COMMAREA)
                   LENGTH(92)
               END-EXEC
           END-IF.

           IF EIBAID = DFHPF3
               EXEC CICS RETURN END-EXEC
           END-IF.

      *    Receive map input
           EXEC CICS
               RECEIVE MAP('FLSRMAP') MAPSET('FLSRMAP')
               INTO(FLSRMAPI)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               PERFORM SHOW-RETURN-FLIGHTS
               IF EIBAID = DFHENTER OR EIBAID = DFHPF2
                   MOVE 'ERROR: SELECT A FLIGHT WITH X' TO MSGO
                   EXEC CICS
                       SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
                       FROM (FLSRMAPO)
                       DATAONLY
                   END-EXEC
               END-IF
               MOVE 'R' TO WS-CA-STATE
               EXEC CICS
                   RETURN TRANSID('FLSR')
                   COMMAREA(WS-COMMAREA)
                   LENGTH(92)
               END-EXEC
           END-IF.

      *    Process AID key
           IF EIBAID = DFHENTER OR EIBAID = DFHPF2
      *        Validate selection before proceeding
               PERFORM VALIDATE-RETURN-SELECTION
               IF WS-SELECTION-MADE = 'Y'
      *            Save selected return flight info
                   MOVE WS-FLT-NUM(WS-SELECTED-ROW)
                       TO WS-CA-RET-FLT
                   MOVE WS-FLT-DEP(WS-SELECTED-ROW)
                       TO WS-CA-RET-DEP
                   MOVE WS-FLT-ARR(WS-SELECTED-ROW)
                       TO WS-CA-RET-ARR

      *            Both flights confirmed - transfer to booking confirmation
                   MOVE 'B' TO WS-CA-STATE
                   EXEC CICS XCTL
                       PROGRAM('BOOK')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                       RESP(WS-RESP)
                   END-EXEC
                   EXEC CICS RETURN END-EXEC
               ELSE
      *            No selection - redisplay with error
                   PERFORM SHOW-RETURN-FLIGHTS
                   MOVE 'ERROR: SELECT A FLIGHT WITH X' TO MSGO
                   EXEC CICS
                       SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
                       FROM (FLSRMAPO)
                       DATAONLY
                   END-EXEC
                   MOVE 'R' TO WS-CA-STATE
                   EXEC CICS
                       RETURN TRANSID('FLSR')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                   END-EXEC
               END-IF
           END-IF.

      *    Default - redisplay screen
           PERFORM SHOW-RETURN-FLIGHTS
           MOVE 'R' TO WS-CA-STATE
           EXEC CICS
               RETURN TRANSID('FLSR')
               COMMAREA(WS-COMMAREA)
               LENGTH(92)
           END-EXEC.

      * =======================================================
      *           VALIDATE RETURN SELECTION
      * =======================================================
       VALIDATE-RETURN-SELECTION.
           MOVE 'N' TO WS-SELECTION-MADE
           MOVE 0 TO WS-SELECTED-ROW.

      *    Check each SELECT field
           IF SEL01I = 'X' OR SEL01I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 1 TO WS-SELECTED-ROW
           END-IF.
           IF SEL02I = 'X' OR SEL02I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 2 TO WS-SELECTED-ROW
           END-IF.
           IF SEL03I = 'X' OR SEL03I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 3 TO WS-SELECTED-ROW
           END-IF.
           IF SEL04I = 'X' OR SEL04I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 4 TO WS-SELECTED-ROW
           END-IF.
           IF SEL05I = 'X' OR SEL05I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 5 TO WS-SELECTED-ROW
           END-IF.
           IF SEL06I = 'X' OR SEL06I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 6 TO WS-SELECTED-ROW
           END-IF.
           IF SEL07I = 'X' OR SEL07I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 7 TO WS-SELECTED-ROW
           END-IF.
           IF SEL08I = 'X' OR SEL08I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 8 TO WS-SELECTED-ROW
           END-IF.
           IF SEL09I = 'X' OR SEL09I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 9 TO WS-SELECTED-ROW
           END-IF.
           IF SEL10I = 'X' OR SEL10I = 'x'
               MOVE 'Y' TO WS-SELECTION-MADE
               MOVE 10 TO WS-SELECTED-ROW
           END-IF.

      * =======================================================
      *              READ VSAM FILE AND FILTER
      * =======================================================
       READ-VSAM-FLIGHTS.
           MOVE 'N' TO WS-EOF.
           EXEC CICS
               STARTBR FILE('FLSRFIL')
                       RIDFLD(WS-FLTNUM)
                       RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO FLIGHTS FOUND' TO MSGO
               GO TO READ-VSAM-END
           END-IF.

           PERFORM UNTIL WS-ROW-IX > 10 OR WS-EOF = 'Y'
               EXEC CICS
                   READNEXT FILE('FLSRFIL')
                            INTO(WS-VSAM-REC)
                            RIDFLD(WS-FLTNUM)
                            RESP(WS-RESP)
               END-EXEC

               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'Y' TO WS-EOF
               ELSE
      *            Parse VSAM record
                   MOVE WS-VSAM-REC(1:5) TO WR-FLTNUM
                   MOVE WS-VSAM-REC(6:3) TO WR-ORIG
                   MOVE WS-VSAM-REC(9:8) TO WR-DATE
                   MOVE WS-VSAM-REC(17:3) TO WR-DEST
                   MOVE WS-VSAM-REC(20:4) TO WR-DEPTIME
                   MOVE WS-VSAM-REC(32:4) TO WR-ARRTIME
                   MOVE WS-VSAM-REC(36:3) TO WR-SEATS

      *            Check if record matches filter criteria
                   IF WR-ORIG = WS-ORIG AND
                      WR-DEST = WS-DEST AND
                      WR-DATE = WS-DATE
      *                Store in flight table
                       MOVE WR-FLTNUM TO WS-FLT-NUM(WS-ROW-IX)
                       MOVE WR-DEPTIME TO WS-FLT-DEP(WS-ROW-IX)
                       MOVE WR-ARRTIME TO WS-FLT-ARR(WS-ROW-IX)
      *                Display on screen
                       PERFORM DISPLAY-FLIGHT-ROW
                       ADD 1 TO WS-ROW-IX
                   END-IF
               END-IF
           END-PERFORM.

           EXEC CICS
               ENDBR FILE('FLSRFIL')
           END-EXEC.

       READ-VSAM-END.
           EXIT.

      * =======================================================
      *              DISPLAY ONE FLIGHT ROW
      * =======================================================
       DISPLAY-FLIGHT-ROW.
           EVALUATE WS-ROW-IX
                       WHEN 1
                          MOVE WR-FLTNUM TO FLT01O
                          MOVE WR-DEPTIME(1:2) TO DEP01O(1:2)
                          MOVE ':' TO DEP01O(3:1)
                          MOVE WR-DEPTIME(3:2) TO DEP01O(4:2)
                          MOVE WR-ARRTIME(1:2) TO ARR01O(1:2)
                          MOVE ':' TO ARR01O(3:1)
                          MOVE WR-ARRTIME(3:2) TO ARR01O(4:2)
                       WHEN 2
                           MOVE WR-FLTNUM TO FLT02O
                           MOVE WR-DEPTIME(1:2) TO DEP02O(1:2)
                           MOVE ':' TO DEP02O(3:1)
                           MOVE WR-DEPTIME(3:2) TO DEP02O(4:2)
                           MOVE WR-ARRTIME(1:2) TO ARR02O(1:2)
                           MOVE ':' TO ARR02O(3:1)
                           MOVE WR-ARRTIME(3:2) TO ARR02O(4:2)
                       WHEN 3
                          MOVE WR-FLTNUM TO FLT03O
                          MOVE WR-DEPTIME(1:2) TO DEP03O(1:2)
                          MOVE ':' TO DEP03O(3:1)
                          MOVE WR-DEPTIME(3:2) TO DEP03O(4:2)
                          MOVE WR-ARRTIME(1:2) TO ARR03O(1:2)
                          MOVE ':' TO ARR03O(3:1)
                          MOVE WR-ARRTIME(3:2) TO ARR03O(4:2)
                       WHEN 4
                          MOVE WR-FLTNUM TO FLT04O
                          MOVE WR-DEPTIME(1:2) TO DEP04O(1:2)
                          MOVE ':' TO DEP04O(3:1)
                          MOVE WR-DEPTIME(3:2) TO DEP04O(4:2)
                          MOVE WR-ARRTIME(1:2) TO ARR04O(1:2)
                          MOVE ':' TO ARR04O(3:1)
                          MOVE WR-ARRTIME(3:2) TO ARR04O(4:2)
                       WHEN 5
                          MOVE WR-FLTNUM TO FLT05O
                          MOVE WR-DEPTIME(1:2) TO DEP05O(1:2)
                          MOVE ':' TO DEP05O(3:1)
                          MOVE WR-DEPTIME(3:2) TO DEP05O(4:2)
                          MOVE WR-ARRTIME(1:2) TO ARR05O(1:2)
                          MOVE ':' TO ARR05O(3:1)
                          MOVE WR-ARRTIME(3:2) TO ARR05O(4:2)
                       WHEN 6
                          MOVE WR-FLTNUM TO FLT06O
                          MOVE WR-DEPTIME(1:2) TO DEP06O(1:2)
                          MOVE ':' TO DEP06O(3:1)
                          MOVE WR-DEPTIME(3:2) TO DEP06O(4:2)
                          MOVE WR-ARRTIME(1:2) TO ARR06O(1:2)
                          MOVE ':' TO ARR06O(3:1)
                          MOVE WR-ARRTIME(3:2) TO ARR06O(4:2)
                       WHEN 7
                           MOVE WR-FLTNUM TO FLT07O
                           MOVE WR-DEPTIME(1:2) TO DEP07O(1:2)
                           MOVE ':' TO DEP07O(3:1)
                           MOVE WR-DEPTIME(3:2) TO DEP07O(4:2)
                           MOVE WR-ARRTIME(1:2) TO ARR07O(1:2)
                           MOVE ':' TO ARR07O(3:1)
                           MOVE WR-ARRTIME(3:2) TO ARR07O(4:2)
                       WHEN 8
                           MOVE WR-FLTNUM TO FLT08O
                           MOVE WR-DEPTIME(1:2) TO DEP08O(1:2)
                           MOVE ':' TO DEP08O(3:1)
                           MOVE WR-DEPTIME(3:2) TO DEP08O(4:2)
                           MOVE WR-ARRTIME(1:2) TO ARR08O(1:2)
                           MOVE ':' TO ARR08O(3:1)
                           MOVE WR-ARRTIME(3:2) TO ARR08O(4:2)
                       WHEN 9
                          MOVE WR-FLTNUM TO FLT09O
                          MOVE WR-DEPTIME(1:2) TO DEP09O(1:2)
                          MOVE ':' TO DEP09O(3:1)
                          MOVE WR-DEPTIME(3:2) TO DEP09O(4:2)
                          MOVE WR-ARRTIME(1:2) TO ARR09O(1:2)
                          MOVE ':' TO ARR09O(3:1)
                          MOVE WR-ARRTIME(3:2) TO ARR09O(4:2)
                       WHEN 10
                           MOVE WR-FLTNUM TO FLT10O
                           MOVE WR-DEPTIME(1:2) TO DEP10O(1:2)
                           MOVE ':' TO DEP10O(3:1)
                           MOVE WR-DEPTIME(3:2) TO DEP10O(4:2)
                           MOVE WR-ARRTIME(1:2) TO ARR10O(1:2)
                           MOVE ':' TO ARR10O(3:1)
                           MOVE WR-ARRTIME(3:2) TO ARR10O(4:2)
                     END-EVALUATE.

      * =======================================================
      *              CLEAR ALL FLIGHT ROWS
      * =======================================================
       CLEAR-ALL-ROWS.
           MOVE SPACES TO FLT01O DEP01O ARR01O
           MOVE SPACES TO FLT02O DEP02O ARR02O
           MOVE SPACES TO FLT03O DEP03O ARR03O
           MOVE SPACES TO FLT04O DEP04O ARR04O
           MOVE SPACES TO FLT05O DEP05O ARR05O
           MOVE SPACES TO FLT06O DEP06O ARR06O
           MOVE SPACES TO FLT07O DEP07O ARR07O
           MOVE SPACES TO FLT08O DEP08O ARR08O
           MOVE SPACES TO FLT09O DEP09O ARR09O
           MOVE SPACES TO FLT10O DEP10O ARR10O.

      * =======================================================
      *              SEND ERROR MESSAGE
      * =======================================================
       SEND-ERROR-MESSAGE.
           MOVE LOW-VALUES TO FLSRMAPO
           MOVE 'ERROR: INVALID COMMAREA' TO MSGO.
           EXEC CICS
               SEND MAP('FLSRMAP') MAPSET('FLSRMAP')
               FROM (FLSRMAPO)
               ERASE
           END-EXEC.


       END PROGRAM FLSR.
