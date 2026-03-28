       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPASS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY BPMAPS.

       01 WS-RESP                  PIC S9(8) COMP.
       01 WS-TRANSID               PIC X(4) VALUE 'CKI4'.
       01 WS-MESSAGE               PIC X(70).

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 CA-STATE              PIC X.
          05 CA-NAME               PIC X(18).
          05 CA-PASSPORT           PIC X(16).
          05 CA-ORIG               PIC X(3).
          05 CA-DEST               PIC X(3).
          05 CA-DEPDATE            PIC X(8).
          05 CA-RETDATE            PIC X(8).
          05 CA-TRIPTYPE           PIC X.
          05 CA-OUT-FLT            PIC X(6).
          05 CA-OUT-DEP            PIC X(4).
          05 CA-OUT-ARR            PIC X(4).
          05 CA-RET-FLT            PIC X(6).
          05 CA-RET-DEP            PIC X(4).
          05 CA-RET-ARR            PIC X(4).
          05 CA-PNR                PIC X(6).
          05 CA-BAGGAGE            PIC X(1).
          05 CA-HANDLUGGAGE        PIC X(1).
          05 CA-OUT-SEAT           PIC X(3).
          05 CA-RET-SEAT           PIC X(3).
          05 CA-OUT-BAGGAGE        PIC X(1).
          05 CA-OUT-CARRYON        PIC X(1).
          05 CA-RET-BAGGAGE        PIC X(1).
          05 CA-RET-CARRYON        PIC X(1).

       PROCEDURE DIVISION USING DFHCOMMAREA.

      * =======================================================
      *                   MAIN LOGIC
      * =======================================================
       MAIN-SECTION.

           IF EIBAID = DFHPF3
              EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF1
              EXEC CICS
                 XCTL PROGRAM('CKIN4')
                 COMMAREA(DFHCOMMAREA)
                 LENGTH(104)
              END-EXEC
           END-IF

           MOVE SPACES TO WS-MESSAGE
           IF EIBAID = DFHENTER
              MOVE 'BOARDING PASS DISPLAYED.' TO WS-MESSAGE
           END-IF

           PERFORM BUILD-SCREEN
           PERFORM SEND-SCREEN
           PERFORM RETURN-PROGRAM.

      * =======================================================
      *            BOARDING PASS SCREEN CONTENT
      * =======================================================
       BUILD-SCREEN.
            MOVE LOW-VALUES TO BPMAPSO

           MOVE CA-NAME         TO PNAMEOO
           MOVE CA-OUT-FLT      TO FLTOO
           MOVE CA-DEPDATE      TO DATOO
           MOVE CA-ORIG         TO FROMOO
           MOVE CA-DEST         TO TOOO

           IF CA-OUT-SEAT = SPACES
              MOVE 'N/A' TO SEATOO
           ELSE
              MOVE CA-OUT-SEAT  TO SEATOO
           END-IF

      *    These values are placeholders until gate/boarding data exists.
           MOVE 'A12'           TO GATEOO
           MOVE CA-OUT-DEP      TO BRDOO
           MOVE CA-OUT-DEP      TO DEPOO
           MOVE 'CHECKED-IN'    TO STATOO

           MOVE WS-MESSAGE TO MSGO.

      * =======================================================
      *                 SEND MAP
      * =======================================================
       SEND-SCREEN.
           EXEC CICS
              SEND MAP('BPMAPS') MAPSET('BPMAPS')
              FROM(BPMAPSO)
              ERASE
           END-EXEC.

      * =======================================================
      *                 RETURN TO TRANSID
      * =======================================================
       RETURN-PROGRAM.
           EXEC CICS
              RETURN TRANSID(WS-TRANSID)
              COMMAREA(DFHCOMMAREA)
              LENGTH(104)
           END-EXEC.
