       IDENTIFICATION DIVISION.
       PROGRAM-ID. CKIN4.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY CKI4MAP.

       01 WS-RESP                  PIC S9(8) COMP.
       01 WS-TRANSID               PIC X(4) VALUE 'CKI4'.
       01 WS-MESSAGE               PIC X(70).
       01 WS-FIRST-NAME            PIC X(9).
       01 WS-LAST-NAME             PIC X(9).
       01 WS-TRIP-DESC             PIC X(10).

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
           EXEC CICS
              HANDLE ABEND LABEL(ABEND-HANDLER)
           END-EXEC

           IF EIBCALEN = 0
              EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF3
              EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF1
              EXEC CICS
                 XCTL PROGRAM('CKIN3')
                 COMMAREA(DFHCOMMAREA)
                 LENGTH(104)
              END-EXEC
              EXEC CICS RETURN END-EXEC
           END-IF

           MOVE SPACES TO WS-MESSAGE

      *    First arrival to CKIN4: just show the screen, do not auto-confirm.
           IF CA-STATE NOT = 'G' AND CA-STATE NOT = 'H'
              MOVE 'G' TO CA-STATE
              PERFORM BUILD-CONFIRMATION-SCREEN
              PERFORM SEND-CONFIRMATION-SCREEN
              PERFORM RETURN-TO-CKIN4
           END-IF

      *    First ENTER confirms check-in; second ENTER transfers to BPASS.
           IF EIBAID = DFHENTER
              IF CA-STATE = 'H'
                 PERFORM TRANSFER-TO-BPASS
              ELSE
                 IF CA-STATE = 'G'
                    MOVE 'H' TO CA-STATE
                    MOVE 'FLIGHT HAS BEEN CONFIRMED.' TO WS-MESSAGE
                 END-IF
              END-IF
           END-IF

           PERFORM BUILD-CONFIRMATION-SCREEN
           PERFORM SEND-CONFIRMATION-SCREEN
           PERFORM RETURN-TO-CKIN4.

      * =======================================================
      *               BUILD SCREEN CONTENT
      * =======================================================
       BUILD-CONFIRMATION-SCREEN.
           MOVE LOW-VALUES TO CKI4MAPO

           MOVE CA-NAME(1:9)  TO WS-FIRST-NAME
           MOVE CA-NAME(10:9) TO WS-LAST-NAME

           MOVE WS-FIRST-NAME TO FIRSTNO
           MOVE WS-LAST-NAME  TO LASTNO
           MOVE CA-PNR        TO PNRVO

           IF CA-TRIPTYPE = 'R' OR CA-TRIPTYPE = 'r'
              MOVE 'ROUND-TRIP' TO WS-TRIP-DESC
           ELSE
              MOVE 'ONE-WAY'    TO WS-TRIP-DESC
           END-IF
           MOVE WS-TRIP-DESC TO TRIPVO

           MOVE CA-OUT-FLT      TO DFLTNO
           MOVE CA-ORIG         TO DORIGO
           MOVE CA-DEST         TO DDESTO
           MOVE CA-OUT-DEP      TO DDEPTO
           MOVE CA-OUT-ARR      TO DARRTO
           IF CA-OUT-SEAT = SPACES
              MOVE 'N/A'        TO DSEATO
           ELSE
              MOVE CA-OUT-SEAT  TO DSEATO
           END-IF
           MOVE CA-OUT-BAGGAGE  TO DBAGGO
           MOVE CA-OUT-CARRYON  TO DCARYO

           IF CA-TRIPTYPE = 'R' OR CA-TRIPTYPE = 'r'
              MOVE 'RETURN'       TO RHEADO
              MOVE 'FLIGHT:'      TO RFLBLO
              MOVE 'ORIG:'        TO RORLBLO
              MOVE 'DEST:'        TO RDSLBLO
              MOVE 'DEP:'         TO RTMLBLO
              MOVE 'ARR:'         TO RATLBLO
              MOVE 'SEAT:'        TO RSLBLO
              MOVE 'BAG:'         TO RBLBLO
              MOVE 'CARRY:'       TO RCLBLO
              MOVE CA-RET-FLT     TO RFLTNO
              MOVE CA-DEST        TO RORIGO
              MOVE CA-ORIG        TO RDESTO
              MOVE CA-RET-DEP     TO RDEPTO
              MOVE CA-RET-ARR     TO RARRTO
              IF CA-RET-SEAT = SPACES
                 MOVE 'N/A'       TO RSEATO
              ELSE
                 MOVE CA-RET-SEAT TO RSEATO
              END-IF
              MOVE CA-RET-BAGGAGE TO RBAGGO
              MOVE CA-RET-CARRYON TO RCARYO
           ELSE
              MOVE SPACES TO RHEADO
              MOVE SPACES TO RFLBLO
              MOVE SPACES TO RORLBLO
              MOVE SPACES TO RDSLBLO
              MOVE SPACES TO RTMLBLO
              MOVE SPACES TO RATLBLO
              MOVE SPACES TO RSLBLO
              MOVE SPACES TO RBLBLO
              MOVE SPACES TO RCLBLO
              MOVE SPACES TO RFLTNO
              MOVE SPACES TO RORIGO
              MOVE SPACES TO RDESTO
              MOVE SPACES TO RDEPTO
              MOVE SPACES TO RARRTO
              MOVE SPACES TO RSEATO
              MOVE SPACES TO RBAGGO
              MOVE SPACES TO RCARYO
           END-IF

           MOVE WS-MESSAGE TO MSGO.

      * =======================================================
      *                 SEND MAP
      * =======================================================
       SEND-CONFIRMATION-SCREEN.
           EXEC CICS
              SEND MAP('CKI4MAP') MAPSET('CKI4MAP')
              FROM(CKI4MAPO)
              ERASE
           END-EXEC.

      * =======================================================
      *                 RETURN TO TRANSID
      * =======================================================
       RETURN-TO-CKIN4.
           EXEC CICS
              RETURN TRANSID(WS-TRANSID)
              COMMAREA(DFHCOMMAREA)
              LENGTH(104)
           END-EXEC.

      * =======================================================
      *               TRANSFER TO BPASS
      * =======================================================
       TRANSFER-TO-BPASS.
           MOVE 'F' TO CA-STATE
           EXEC CICS
               XCTL PROGRAM('BPASS')
                    COMMAREA(DFHCOMMAREA)
                    LENGTH(104)
           END-EXEC.

      * =======================================================
      *              ABEND RECOVERY HANDLER
      * =======================================================
       ABEND-HANDLER.
                MOVE 'H' TO CA-STATE
                MOVE 'BPASS TRANSFER FAILED (APCT).' TO WS-MESSAGE
                PERFORM BUILD-CONFIRMATION-SCREEN
                PERFORM SEND-CONFIRMATION-SCREEN
                PERFORM RETURN-TO-CKIN4.
