//KC03BAEC JOB
//*
//CICSPROC JCLLIB ORDER=(KC03BAE.CICS.PROCLIB)
//*
//*********************************************************************
//*           TRANSLATE COMPILE
//*           AND LINK A CICS COBOL PROGRAM - CKIN
//*********************************************************************
//COMPLI   EXEC DFHZITCL,
//             PROGLIB=TSOECCC.CICSTS12.STUDENT.LOADLIB,
//             PROGSRC=KC03BAE.CICS.CBL.NEW,
//             PROGMBR=CKIN,
//             CPYLIBWS=KC03BAE.CICS.COPYLIB,
//             CPYLIBPR=KC03BAE.CICS.COPYLIB
//* NAME OF PROGRAM AND MEMBER TO TRANSLATE/COMPILE/LKED
//COBOL.SYSIN DD DSN=&PROGSRC(&PROGMBR),DISP=SHR