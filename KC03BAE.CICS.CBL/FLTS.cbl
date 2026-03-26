IDENTIFICATION DIVISION.
       PROGRAM-ID. FLTS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * =======================================================
      *                   COPY LIBRARY
      * =======================================================
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY INTRO.
       COPY FLTSMAP.

      * =======================================================
      *                      VALUES
      * =======================================================
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
       01 WS-NAME-DIGITS           PIC 9(4) COMP VALUE 0.
       01 WS-I                     PIC 9(4) COMP VALUE 0.
       01 WS-NUMERIC-FOUND         PIC X VALUE 'N'.
       01 WS-NUMP-VALUE            PIC 9(2) VALUE 0.
       01 WS-DOB-DIGIT-COUNT       PIC 9(4) COMP VALUE 0.
       01 WS-DOB-SLASH-COUNT       PIC 9(4) COMP VALUE 0.
       01 WS-DOB-DAY               PIC 99.
       01 WS-DOB-MONTH             PIC 99.
       01 WS-DOB-YEAR              PIC 9(4).
       01 WS-CURRENT-YEAR          PIC 9(4) VALUE 2026.
       01 WS-AGE                   PIC 999.
       01 WS-PASSPORT-LEN          PIC 9(2) COMP VALUE 0.
       01 WS-DEP-DATE-DAY          PIC 99.
       01 WS-DEP-DATE-MONTH        PIC 99.
       01 WS-DEP-DATE-YEAR         PIC 9(4).
       01 WS-RET-DATE-DAY          PIC 99.
       01 WS-RET-DATE-MONTH        PIC 99.
       01 WS-RET-DATE-YEAR         PIC 9(4).
       01 WS-SHOW-DATE-FIELDS      PIC X VALUE 'N'.

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

           IF EIBCALEN = 0
               MOVE 'I' TO WS-STATE
           ELSE
               MOVE CA-STATE TO WS-STATE
           END-IF

           EVALUATE WS-STATE
               WHEN 'I'
                   EXEC CICS
                       SEND MAP('INTRO') MAPSET('INTRO') MAPONLY ERASE
                   END-EXEC

                   MOVE 'F' TO WS-STATE
                   MOVE WS-STATE TO WS-CA-STATE
                   EXEC CICS
                       RETURN TRANSID('FLTS')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                   END-EXEC

               WHEN 'F'
                   IF EIBAID = DFHPF3
                       EXEC CICS RETURN END-EXEC
                   END-IF

                   IF EIBAID = DFHENTER
                       PERFORM CLEAR-INPUT-FIELDS
                       EXEC CICS
                           SEND MAP('FLTSMAP')
                           MAPSET('FLTSMAP')
                           ERASE
                       END-EXEC

                       PERFORM DISPLAY-DATE-FIELDS
                       EXEC CICS
                           SEND MAP('FLTSMAP')
                           MAPSET('FLTSMAP')
                           DATAONLY
                       END-EXEC

                       MOVE 'M' TO WS-STATE
                       MOVE WS-STATE TO WS-CA-STATE
                       EXEC CICS
                           RETURN TRANSID('FLTS')
                           COMMAREA(WS-COMMAREA)
                           LENGTH(92)
                       END-EXEC
                   END-IF
                   MOVE WS-STATE TO WS-CA-STATE
                   EXEC CICS
                       RETURN TRANSID('FLTS')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                   END-EXEC

               WHEN 'M'
                   EXEC CICS
                       RECEIVE MAP('FLTSMAP') MAPSET('FLTSMAP')
                       RESP(WS-RESP)
                   END-EXEC

                   IF WS-RESP = DFHRESP(MAPFAIL)
                       PERFORM SEND-INITIAL-SCREEN
                   ELSE
                       PERFORM HANDLE-AID-KEYS
                   END-IF

                   MOVE WS-STATE TO WS-CA-STATE
                   EXEC CICS
                       RETURN TRANSID('FLTS')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                   END-EXEC
           END-EVALUATE.

      * =======================================================
      *             HANDLE AID KEYS - F1 & F3
      * =======================================================
       HANDLE-AID-KEYS.

           IF EIBAID = DFHPF3
               EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHPF1
               EXEC CICS
                 SEND CONTROL ERASE
               END-EXEC

               EXEC CICS RETURN END-EXEC
           END-IF.

           IF EIBAID = DFHENTER
      *        Validate all input
               PERFORM VALIDATE-INPUT

               IF WS-VALID-FLAG = 'N'
                 EXEC CICS
                     SEND MAP('FLTSMAP') MAPSET('FLTSMAP') DATAONLY
                 END-EXEC
               ELSE
      *            Prepare data for FLSR screen
                   MOVE 'S' TO WS-CA-STATE
                   MOVE NAMEINI TO WS-CA-NAME
                   MOVE PASSINI TO WS-CA-PASSPORT
                   MOVE ORIGININI TO WS-CA-ORIG
                   MOVE DESTINI TO WS-CA-DEST
                   MOVE TRIPINI TO WS-CA-TRIPTYPE
                   MOVE SPACES TO WS-CA-PNR
      *            Convert DDMMYYYY to YYYYMMDD for departure date
                   IF DEPDTI NOT = ZERO AND DEPDTI NOT = SPACES
                       STRING DEPDTI(5:4) DEPDTI(3:2) DEPDTI(1:2)
                           DELIMITED BY SIZE INTO WS-CA-DEPDATE
                   ELSE
                       MOVE SPACES TO WS-CA-DEPDATE
                   END-IF
      *            Convert DDMMYYYY to YYYYMMDD for return date
                   IF RETDTI NOT = ZERO AND RETDTI NOT = SPACES
                       STRING RETDTI(5:4) RETDTI(3:2) RETDTI(1:2)
                           DELIMITED BY SIZE INTO WS-CA-RETDATE
                   ELSE
                       MOVE SPACES TO WS-CA-RETDATE
                   END-IF
      *            Transfer to flight search results
                   EXEC CICS XCTL
                       PROGRAM('FLSR')
                       COMMAREA(WS-COMMAREA)
                       LENGTH(92)
                       RESP(WS-RESP)
                   END-EXEC
                   IF WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'ERROR: CANNOT TRANSFER TO FLSR'
                           TO MSGO
                       EXEC CICS
                           SEND MAP('FLTSMAP')
                           MAPSET('FLTSMAP')
                           DATAONLY
                       END-EXEC
                   END-IF
               END-IF
           END-IF.

      * =======================================================
      *                   INPUT VALIDATION
      * =======================================================
       VALIDATE-INPUT.

           MOVE 'Y' TO WS-VALID-FLAG
           MOVE SPACES TO MSGO
           PERFORM VALIDATE-NAME
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-DOB
           END-IF
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-PASSWORD
           END-IF
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-ORIGIN-DEST
           END-IF
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-NUMP
           END-IF
           IF WS-VALID-FLAG = 'Y'
               PERFORM VALIDATE-TRIP
           END-IF

           IF WS-VALID-FLAG = 'Y' AND
              (TRIPINI = 'O' OR TRIPINI = 'R')
               PERFORM VALIDATE-TRAVEL-DATES
           END-IF.

      * =======================================================
      *                   NAME VALIDATION
      * =======================================================
       VALIDATE-NAME.

           IF NAMEINI = SPACES OR NAMEINL = 0
              OR FUNCTION TRIM(NAMEINI) = SPACES
               MOVE 'ERROR: NAME CANNOT BE EMPTY' TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           ELSE
               MOVE 'N' TO WS-NUMERIC-FOUND
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LENGTH OF NAMEINI
                   IF NAMEINI(WS-I:1) IS NUMERIC
                       MOVE 'Y' TO WS-NUMERIC-FOUND
                   END-IF
               END-PERFORM

               IF WS-NUMERIC-FOUND = 'Y'
                   MOVE 'ERROR: NAME CANNOT CONTAIN DIGITS'
                       TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               ELSE
                   PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > LENGTH OF NAMEINI
                       IF NAMEINI(WS-I:1) NOT = SPACE
                           AND NAMEINI(WS-I:1) NOT = '-'
                           AND NAMEINI(WS-I:1) IS NOT ALPHABETIC
                           MOVE 'ERROR: NAME CAN ONLY CONTAIN LETTERS'
                               TO MSGO
                           MOVE 'N' TO WS-VALID-FLAG
                       END-IF
                   END-PERFORM
               END-IF
           END-IF.

      * =======================================================
      *                   DOB VALIDATION
      * =======================================================
       VALIDATE-DOB.

           IF DOBINL = 0 OR DOBINI = 0
               MOVE 'ERROR: DATE OF BIRTH CANNOT BE EMPTY'
                   TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           ELSE
      *        Extract day, month, year from DOB (DDMMYYYY)
               IF DOBINL >= 8
                   MOVE DOBINI(1:2) TO WS-DOB-DAY
                   MOVE DOBINI(3:2) TO WS-DOB-MONTH
                   MOVE DOBINI(5:4) TO WS-DOB-YEAR

      *            Validate day range
                   IF WS-VALID-FLAG = 'Y'
                       IF WS-DOB-DAY < 1 OR WS-DOB-DAY > 31
                           MOVE 'ERROR: BIRTH DAY MUST BE 1-31'
                               TO MSGO
                           MOVE 'N' TO WS-VALID-FLAG
                       END-IF

      *            Validate month range
                   IF WS-DOB-MONTH < 1 OR WS-DOB-MONTH > 12
                       MOVE 'ERROR: BIRTH MONTH MUST BE 1-12' TO MSGO
                       MOVE 'N' TO WS-VALID-FLAG
                   END-IF
                   END-IF

      *            Check not in future
                   IF WS-VALID-FLAG = 'Y'
                       IF WS-DOB-YEAR > WS-CURRENT-YEAR
                           MOVE 'ERROR: DOB CANNOT BE IN FUTURE'
                               TO MSGO
                           MOVE 'N' TO WS-VALID-FLAG
                       END-IF
                   END-IF

      *            Check age 18 or older
                   IF WS-VALID-FLAG = 'Y'
                       COMPUTE WS-AGE = WS-CURRENT-YEAR - WS-DOB-YEAR
                       IF WS-AGE < 18
                           MOVE 'ERROR: MUST BE 18 YEARS OR OLDER'
                               TO MSGO
                           MOVE 'N' TO WS-VALID-FLAG
                       END-IF
                   END-IF
               ELSE
                   MOVE 'ERROR: DOB FORMAT MUST BE DDMMYYYY' TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF
           END-IF.

      * =======================================================
      *                PASSWORD VALIDATION
      * =======================================================
       VALIDATE-PASSWORD.

           IF PASSINI = SPACES OR PASSINL = 0
              OR FUNCTION TRIM(PASSINI) = SPACES
               MOVE 'ERROR: PASSPORT NO CANNOT BE EMPTY' TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           ELSE
      *        Check passport length (9-12 characters)
      *        Count actual characters excluding trailing spaces
               COMPUTE WS-PASSPORT-LEN =
                   FUNCTION LENGTH(FUNCTION TRIM(PASSINI))
               IF WS-PASSPORT-LEN < 9
                   MOVE 'ERROR: PASSPORT MUST BE AT LEAST 9 CHARS'
                       TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               ELSE
               IF WS-VALID-FLAG = 'Y' AND WS-PASSPORT-LEN > 12
                   MOVE 'ERROR: PASSPORT CANNOT EXCEED 12 CHARS'
                       TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF
           END-IF.

      * =======================================================
      *              ORIGIN/DESTINATION VALIDATION
      * =======================================================
       VALIDATE-ORIGIN-DEST.

           IF ORIGININI = SPACES OR ORIGININL = 0
              OR DESTINI = SPACES OR DESTINL = 0
              OR FUNCTION TRIM(ORIGININI) = SPACES
              OR FUNCTION TRIM(DESTINI) = SPACES
               MOVE 'ERROR: ORIGIN AND DESTINATION REQUIRED'
                   TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           END-IF.

      * =======================================================
      *             NUMBER OF PASSENGERS VALIDATION
      * =======================================================
       VALIDATE-NUMP.

           IF NUMPINI = SPACES OR NUMPINL = 0
              OR FUNCTION TRIM(NUMPINI) = SPACES
               MOVE 'ERROR: NUMBER OF PASSENGERS REQUIRED'
                   TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           ELSE
      *        Check each character to ensure only digits
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LENGTH OF NUMPINI OR WS-VALID-FLAG = 'N'
                   IF NUMPINI(WS-I:1) NOT = SPACE
                       IF NUMPINI(WS-I:1) NOT NUMERIC
                           MOVE 'ERROR: PASSENGERS MUST BE NUMERIC ONLY'
                               TO MSGO
                           MOVE 'N' TO WS-VALID-FLAG
                       END-IF
                   END-IF
               END-PERFORM

      *        If characters are valid, check numeric range
               IF WS-VALID-FLAG = 'Y'
                   IF FUNCTION TRIM(NUMPINI) NOT NUMERIC
                       MOVE 'ERROR: PASSENGERS MUST BE NUMERIC' TO MSGO
                       MOVE 'N' TO WS-VALID-FLAG
                   ELSE
                       COMPUTE WS-NUMP-VALUE =
                           FUNCTION NUMVAL(FUNCTION TRIM(NUMPINI))
                       IF WS-NUMP-VALUE < 1 OR WS-NUMP-VALUE > 9
                           MOVE 'ERROR: PASSENGERS MUST BE 1-9' TO MSGO
                           MOVE 'N' TO WS-VALID-FLAG
                       END-IF
                   END-IF
               END-IF
           END-IF.

      * =======================================================
      *                   TRIP TYPE VALIDATION
      * =======================================================
       VALIDATE-TRIP.

           IF TRIPINI = SPACES OR TRIPINL = 0
              OR FUNCTION TRIM(TRIPINI) = SPACES
               MOVE 'ERROR: TRIP TYPE REQUIRED' TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           ELSE
               IF TRIPINI NOT = 'O' AND TRIPINI NOT = 'R'
                   MOVE 'ERROR: TRIP TYPE MUST BE O OR R' TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF
           END-IF.

      * =======================================================
      *               TRAVEL DATES VALIDATION
      * =======================================================
       VALIDATE-TRAVEL-DATES.

           IF DEPDTL = 0 OR DEPDTI = 0
               MOVE 'ERROR: DEPARTURE DATE IS REQUIRED' TO MSGO
               MOVE 'N' TO WS-VALID-FLAG
           ELSE
               MOVE DEPDTI(1:2) TO WS-DEP-DATE-DAY
               MOVE DEPDTI(3:2) TO WS-DEP-DATE-MONTH
               MOVE DEPDTI(5:4) TO WS-DEP-DATE-YEAR

               IF WS-DEP-DATE-MONTH < 1 OR WS-DEP-DATE-MONTH > 12
                   MOVE 'ERROR: DEP MONTH MUST BE 1-12' TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF

               IF WS-DEP-DATE-DAY < 1 OR WS-DEP-DATE-DAY > 31
                   MOVE 'ERROR: DEP DAY MUST BE 1-31' TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF
           END-IF.

           IF TRIPINI = 'R'
               IF RETDTL = 0 OR RETDTI = 0
                   MOVE 'ERROR: RETURN DATE IS REQUIRED' TO MSGO
                   MOVE 'N' TO WS-VALID-FLAG
               ELSE
                   MOVE RETDTI(1:2) TO WS-RET-DATE-DAY
                   MOVE RETDTI(3:2) TO WS-RET-DATE-MONTH
                   MOVE RETDTI(5:4) TO WS-RET-DATE-YEAR

                   IF WS-RET-DATE-MONTH < 1 OR WS-RET-DATE-MONTH > 12
                       MOVE 'ERROR: RET MONTH MUST BE 1-12' TO MSGO
                       MOVE 'N' TO WS-VALID-FLAG
                   END-IF

                   IF WS-RET-DATE-DAY < 1 OR WS-RET-DATE-DAY > 31
                       MOVE 'ERROR: RET DAY MUST BE 1-31' TO MSGO
                       MOVE 'N' TO WS-VALID-FLAG
                   END-IF

                   IF WS-RET-DATE-YEAR < WS-DEP-DATE-YEAR OR
                      (WS-RET-DATE-YEAR = WS-DEP-DATE-YEAR AND
                       WS-RET-DATE-MONTH < WS-DEP-DATE-MONTH) OR
                      (WS-RET-DATE-YEAR = WS-DEP-DATE-YEAR AND
                       WS-RET-DATE-MONTH = WS-DEP-DATE-MONTH AND
                       WS-RET-DATE-DAY <= WS-DEP-DATE-DAY)
                       MOVE 'ERROR: RET DATE MUST BE AFTER DEP DATE'
                           TO MSGO
                       MOVE 'N' TO WS-VALID-FLAG
                   END-IF
               END-IF
           END-IF.


       SEND-INITIAL-SCREEN.

           MOVE SPACES TO NAMEINO
           MOVE SPACES TO MSGO
           PERFORM CLEAR-INPUT-FIELDS

           EXEC CICS
               SEND MAP('FLTSMAP') MAPSET('FLTSMAP') ERASE
           END-EXEC

           PERFORM DISPLAY-DATE-FIELDS
           EXEC CICS
               SEND MAP('FLTSMAP') MAPSET('FLTSMAP') DATAONLY
           END-EXEC.

      * =======================================================
      *                   CLEAR INPUT FIELDS
      * =======================================================
       CLEAR-INPUT-FIELDS.
           move low-values to fltsmapo.
           MOVE SPACES TO NAMEINI
           MOVE SPACES TO PASSINI
           MOVE ZERO TO DOBINI
           MOVE 'DDMMYYYY' TO DOBINO
           MOVE SPACES TO ORIGININI
           MOVE SPACES TO DESTINI
           MOVE SPACES TO NUMPINI
           MOVE SPACES TO TRIPINI
           MOVE ZERO TO DEPDTI
           MOVE ZERO TO RETDTI.

      * =======================================================
      *            HIDE DATE FIELDS INITIALLY
      * =======================================================
       HIDE-DATE-FIELDS.

           MOVE -1 TO LBLDEPDTL
           MOVE DFHBMASK TO LBLDEPDTA
           MOVE -1 TO DEPDTL
           MOVE DFHBMASK TO DEPDTA
           MOVE -1 TO LBLRETDTL
           MOVE DFHBMASK TO LBLRETDTA
           MOVE -1 TO RETDTL
           MOVE DFHBMASK TO RETDTA.

      * =======================================================
      *         DISPLAY DATE FIELDS BASED ON TRIP TYPE
      * =======================================================
       DISPLAY-DATE-FIELDS.

           MOVE -1 TO LBLDEPDTL
           MOVE DFHBMPEM TO LBLDEPDTA
           MOVE -1 TO DEPDTL
           MOVE DFHBMPEM TO DEPDTA
           MOVE 'DDMMYYYY' TO DEPDTO

      *    Always show both date fields
           MOVE -1 TO LBLRETDTL
           MOVE DFHBMPEM TO LBLRETDTA
           MOVE -1 TO RETDTL
           MOVE DFHBMPEM TO RETDTA
           MOVE 'DDMMYYYY' TO RETDTO.


       END PROGRAM FLTS.