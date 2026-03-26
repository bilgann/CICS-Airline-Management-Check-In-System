//CMFCCBAE JOB (KC03BAE),'VSAM DEFINE',
//             CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID
//*
//* DEFINE VSAM KSDS FOR FLIGHT DATA
//*
//STEP1   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE KC03BAE.CICS.FLSR CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0

  DEFINE CLUSTER                           -
    (NAME(KC03BAE.CICS.FLSR)               -
     INDEXED                               -
     KEYS(5 0)                             -
     RECORDSIZE(38 38)                     -
     TRACKS(5 5)                           -
     FREESPACE(20 10)                      -
     SHAREOPTIONS(2 3))                    -
    DATA                                   -
    (NAME(KC03BAE.CICS.FLSR.DATA))         -
    INDEX                                  -
    (NAME(KC03BAE.CICS.FLSR.INDEX))
/*
