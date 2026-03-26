//FLTSJOB  JOB (KC03BAE),'CICS COMP',
//             CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
//*
//* COMPILE AND LINK FOR CICS COBOL PROGRAM FLTS
//*
//**********************************************************
//* VERIFY YOUR CICS AND COBOL LIBRARY NAMES BELOW
//**********************************************************
// SET CICS=DFH610.CICS
// SET LE=CEE
// SET COB=IGY.V6R2M0
// SET USRPFX=KC03BAE.CICS
//*
//**********************************************************
//* 1. CICS TRANSLATOR
//**********************************************************
//TRN      EXEC PGM=DFHECP1$
//STEPLIB  DD  DSN=&CICS..SDFHLOAD,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DSN=&&SYSPUNCH,
//             DISP=(,PASS),
//             UNIT=SYSDA,
//             SPACE=(CYL,(1,1))
//*
//* INPUT SOURCE: ASSUMING FLTS IS A MEMBER OF KC03BAE.CICS.CBL
//SYSIN    DD  DSN=&USRPFX..CBL(FLTS),DISP=SHR
//*
//**********************************************************
//* 2. COBOL COMPILER
//**********************************************************
//COB      EXEC PGM=IGYCRCTL,
//             PARM='NODECK,OBJECT,LIB,APOST,XREF,DYNAM'
//STEPLIB  DD  DSN=&COB..SIGYCOMP,DISP=SHR
//SYSLIB   DD  DSN=&CICS..SDFHCOB,DISP=SHR
//         DD  DSN=&CICS..SDFHMAC,DISP=SHR
//         DD  DSN=&USRPFX..COPYLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&LOADSET,
//             DISP=(,PASS),
//             UNIT=SYSDA,
//             SPACE=(CYL,(1,1))
//SYSIN    DD  DSN=&&SYSPUNCH,DISP=(OLD,DELETE)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//*
//**********************************************************
//* 3. LINKAGE EDITOR
//**********************************************************
//LKED     EXEC PGM=IEWL,PARM='LIST,XREF,LET,MAP,RENT'
//SYSLIB   DD  DSN=&CICS..SDFHLOAD,DISP=SHR
//         DD  DSN=&LE..SCEELKED,DISP=SHR
//SYSLMOD  DD  DSN=&USRPFX..LOAD(FLTS),DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  *
  INCLUDE SYSLIB(DFHELII)
  NAME FLTS(R)
/*
