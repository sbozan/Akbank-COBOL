//SORTEG03 JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
    DELETE Z95737.QSAM.INP NONVSAM
    IF LASTCC LE 08 THEN SET MAXCC = 00
//SORT0200 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD *
10002949
10002840
10002978
20003215
30255618
//SORTOUT  DD DSN=Z95737.QSAM.INP,
// DISP=(NEW,CATLG,DELETE),
// SPACE=(TRK,(5,5),RLSE),
// DCB=(RECFM=FB,LRECL=8)
//SYSIN    DD *
    SORT FIELDS=COPY