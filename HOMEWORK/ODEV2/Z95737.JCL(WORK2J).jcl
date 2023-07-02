//WORK2J JOB 1,NOTIFY=&SYSUID
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(WORK2),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(WORK2),DISP=SHR
//*------------------------------------------------------------/
// IF RC < 5 THEN
//*------------------------------------------------------------/
//RUN        EXEC PGM=WORK2
//STEPLIB      DD DSN=&SYSUID..LOAD,DISP=SHR
//INPFILE      DD DSN=&SYSUID..QSAM.BB,DISP=SHR
//OUTPFILE     DD DSN=&SYSUID..QSAM.CC,DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(20,20),RLSE),
//             DCB=(RECFM=FB,LRECL=55,BLKSIZE=0),UNIT=3390
//SYSOUT       DD SYSOUT=*,OUTLIM=15000
//CEEDUMP      DD DUMMY
//SYSUDUMP     DD DUMMY
// ELSE
// ENDIF
